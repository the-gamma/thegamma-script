module TheGamma.Editors

open TheGamma
open TheGamma.AstOperations
open Fable.Import
open Fable.Extensions

type Property = 
  | Property of string * Schema option * Type

type Editor = 
  | SingleChoice of Name * Property[]
  | NestedChoice of Name * Name * (Property * Property[])[]
  | CreateList of Name * Name[] * Property[]
  member x.Range = 
    match x with
    | SingleChoice(n, _) -> n.Range
    | NestedChoice(n1, n2, _) -> Ranges.unionRanges n1.Range n2.Range
    | CreateList(n, n2, _) -> n2 |> Array.fold (fun r n -> Ranges.unionRanges r n.Range) n.Range 

let rec getMembers typ = async {
  match typ with
  | Type.Object(o) -> 
      return o.Members
  | Type.Delayed(_, f) ->
      let! typ = Async.AwaitFuture f
      return! getMembers typ 
  | _ -> return failwith "Not an object" }

let rec listsEqual l1 l2 f = 
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys when f x y -> listsEqual xs ys f
  | _ -> false 

let rec arraysEqual (l1:_[]) (l2:_[]) f = 
  let rec loop i =
    if i = l1.Length && i = l2.Length then true
    elif i < l1.Length && i < l2.Length then 
      f (l1.[i]) (l2.[i]) && loop (i+1)
    else false
  loop 0

let rec typesEqual t1 t2 = 
  match t1, t2 with
  | Type.Any, Type.Any -> true
  | Type.Delayed(g1, _), Type.Delayed(g2, _) -> g1 = g2
  | Type.Function(a1, r1), Type.Function(a2, r2) -> 
      listsEqual (r1::a1) (r2::a2) typesEqual
  | Type.Object(o1), Type.Object(o2) -> 
      arraysEqual o1.Members o2.Members (fun m1 m2 ->
          match m1, m2 with 
          | Member.Property(n1, t1, s1, _), Member.Property(n2, t2, s2, _) -> n1 = n2 && s1 = s2 && typesEqual t1 t2
          | Member.Method(n1, a1, r1, _), Member.Method(n2, a2, r2, _) -> 
              n1 = n2 && typesEqual r1 r2 && 
                listsEqual a1 a2 (fun (s1, b1, t1) (s2, b2, t2) -> 
                  s1 = s2 && b1 = b2 && typesEqual t1 t2)
          | _ -> false)
  | Type.Parameter n1, Type.Parameter n2 -> n1 = n2
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2
  | Type.Unit, Type.Unit -> true
  | _ -> false

let getProperty (name:Name) members = 
  members |> Array.tryPick (function 
    | Member.Property(name=n; schema=s; typ=t) when n=name.Name -> Some(s, t) 
    | _ -> None)

let filterProperties f members = async {
  let filtered = members |> Array.choose (function 
    | Member.Property(name=n; schema=s; typ=t) when f (n, s, t) -> Some(Property.Property(n, s, t))
    | _ -> None)
  return filtered }

let dominant all subset =
  let nall = Seq.length all
  let nsub = Seq.length subset
  nsub > 2 && nsub >= nall * 2 / 3

let chooseableProperty equalTyp (name:Name) typ = async {
  let! members = getMembers typ
  match getProperty name members with
  | Some(Some propSchema, propTyp) ->
      let! alts = members |> filterProperties (function
        | _, Some s, t -> s.Type = propSchema.Type && (not equalTyp || typesEqual t propTyp)
        | _ -> false )
      if dominant members alts then return Some(name, alts)
      else return None 
  | _ -> return None }

/// Walk over expression and call 'f' with all property chain suffixes.
/// Given `a.b.c`, it will be called with [c], [b;c], [a;b;c].
let pickChainSuffixes f expr = 
  let rec loop res suffix expr = async {
    match expr.Expr with
    | ExprKind.Property(inst, name) ->
        let! members = getMembers inst.Type
        match getProperty name members with
        | Some(propSch, propTy) ->
            let suffix = (inst.Type, name, propSch, propTy)::suffix
            let! picked = f suffix
            match picked with 
            | Some newRes -> return! loop (newRes::res) suffix inst
            | _ -> return! loop res suffix inst 
        | None -> return! loop res suffix inst
    | ExprNode(es, _) -> return! es |> Async.fold (fun st e -> loop st [] e) res 
    | ExprLeaf -> return res }
  loop [] [] expr


let collectSingleChoiceEditors =
  pickChainSuffixes (fun chain -> async {
    match chain with
    | (ty, name, _, _)::_ -> 
        let! ed = chooseableProperty true name ty 
        return Option.map SingleChoice ed
    | _ -> return None })

let collectNestedChoiceEditors = 
  pickChainSuffixes (fun chain -> async {
    match chain with
    | (catParentTy, catName, catSch, catTy)::(valParentTy, valName, (Some valSch), valTy)::_ ->
        Log.trace("editors", "checking %s.%s", catName.Name, valName.Name)
        let! cat = chooseableProperty false catName catParentTy
        match cat with
        | Some(catName, catMembers) ->
            Log.trace("editors", "collecting %s nested members", catMembers.Length)
            let! nestedMembers = catMembers |> Async.Array.map (fun (Property(n, _, t) as p) -> async {
              let! members = getMembers t
              let! filtered = members |> filterProperties (function
                | (n, Some s, t) -> s.Type = valSch.Type && typesEqual t valTy
                | _ -> false )
              return p, (members, filtered) })
            if dominant (Seq.collect (snd >> fst) nestedMembers) (Seq.collect (snd >> snd) nestedMembers) then
              let props = nestedMembers |> Array.map (fun (p, (_, filtered)) ->
                p, filtered)
              return Some(NestedChoice(catName, valName, props))
            else return None
        | _ -> return None
    | _ -> return None })

type ItemListSchema = { name : string }
type CreateActionSchema = { result : ItemListSchema }
type AddActionSchema = { targetCollection : ItemListSchema }

let collectItemListEditors =
  pickChainSuffixes (fun chain -> async {
    match chain with
    | (caParentTy, caName, Some caSch, caTy)::addActions when caSch.Type = "CreateAction" ->
        let listName = (unbox<CreateActionSchema> caSch.JSON).result.name

        /// Collect all AddActions in the rest of the chain and return
        /// the added options together with the type of the last member 
        let rec collectAdds added lastTy = function
          | (addParentTy, addName, Some addSch, addTy)::addActions when 
                addSch.Type = "AddAction" &&
                listName = (unbox<AddActionSchema> addSch.JSON).targetCollection.name ->
              collectAdds (addName::added) addTy addActions
          | _ -> List.rev added, lastTy

        // Collect add actions from the rest of the chain & get available
        // actions of the last type in the chain (that can be added)
        let adds, lastTy = collectAdds [] caTy addActions
        let! members = getMembers lastTy
        let! availableAdds = members |> filterProperties (function
          | (n, Some s, t) when s.Type = "AddAction" -> (unbox<AddActionSchema> s.JSON).targetCollection.name = listName
          | _ -> false )
        return Some(CreateList(caName, Array.ofList adds, availableAdds))
    | _ -> return None })

let collectCmdEditors (cmd:Command<_>) = async {
  match cmd.Command with 
  | CommandKind.Let(_, e)
  | CommandKind.Expr e -> 
      Log.trace("editors", "single choice")
      let! single = collectSingleChoiceEditors e
      Log.trace("editors", "item list")
      let! itemList = collectItemListEditors e
      Log.trace("editors", "multi choice")
      let! nested = collectNestedChoiceEditors e
      return single @ nested @ itemList  }  
