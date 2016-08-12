module TheGamma.Editors

open TheGamma
open TheGamma.AstOperations
open TheGamma.TypeChecker
open Fable.Import
open Fable.Extensions

// ------------------------------------------------------------------------------------------------
// Finding editor components in code
// ------------------------------------------------------------------------------------------------

type Property = 
  | Property of string * Schema option * Type

type Editor = 
  | SingleChoice of Documentation * Name * Property[]
  | NestedChoice of Documentation * Documentation * Name * Name * (Property * Property[])[]
  | CreateList of Documentation * Name * Name[] * Property[]
  member x.Range = 
    match x with
    | SingleChoice(_, n, _) -> n.Range
    | NestedChoice(_, _, n1, n2, _) -> Ranges.unionRanges n1.Range n2.Range
    | CreateList(_, n, n2, _) -> n2 |> Array.fold (fun r n -> Ranges.unionRanges r n.Range) { n.Range with Start = n.Range.End }

let rec getMembers typ = async {
  match typ with
  | Type.Object(o) -> 
      return o.Members
  | Type.Delayed(_, f) ->
      let! typ = Async.AwaitFuture f
      return! getMembers typ 
  | _ -> 
    Log.error("editors", "getMembers: Type %O is not an object", typ)
    return failwith "getMembers: Not an object" }

let getProperty (name:Name) members = 
  members |> Array.tryPick (function 
    | Member.Property(name=n; schema=s; typ=t; docs=d) when n=name.Name -> Some(s, t, d) 
    | _ -> None)

let filterProperties f members = 
  let filtered = members |> Array.choose (function 
    | Member.Property(name=n; schema=s; typ=t) when f (n, s, t) -> Some(Property.Property(n, s, t))
    | _ -> None)
  filtered 

let dominant all subset =
  let nall = Seq.length all
  let nsub = Seq.length subset
  nsub >= 2 && nsub >= nall * 2 / 3

let chooseableProperty equalTyp (name:Name) typ = async {
  let! members = getMembers typ
  match getProperty name members with
  | Some(Some propSchema, propTyp, _) ->
      let alts = members |> filterProperties (function
        | _, Some s, t -> s.Type = propSchema.Type && (not equalTyp || TypeChecker.typesEqual t propTyp)
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
        | Some(propSch, propTy, propDoc) ->
            let suffix = (inst.Type, name, propSch, propTy, propDoc)::suffix
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
    | (tyParent, name, _, _, doc)::_ -> 
        let! ed = chooseableProperty true name tyParent
        return Option.map (fun (n, p) -> SingleChoice(doc, n, p)) ed
    | _ -> return None })

let collectNestedChoiceEditors = 
  pickChainSuffixes (fun chain -> async {
    match chain with
    | (catParentTy, catName, catSch, catTy, catDoc)::
          (valParentTy, valName, (Some valSch), valTy, valDoc)::_ ->
        Log.trace("editors", "checking %s.%s", catName.Name, valName.Name)
        let! catp = chooseableProperty false catName catParentTy
        let! valp = chooseableProperty true valName valParentTy
        match catp, valp with
        | Some(catName, catMembers), Some(valName, valMembers) ->
            Log.trace("editors", "collecting %s nested members", catMembers.Length)
            let nestedMembers trunc = 
              catMembers |> trunc |> Async.Array.map (fun (Property(n, _, t) as p) -> async {
              let! members = getMembers t
              let filtered = members |> filterProperties (function
                | (n, Some s, t) -> s.Type = valSch.Type && TypeChecker.typesEqual t valTy
                | _ -> false )
              return p, (members, filtered) })
            let! checkMembers = nestedMembers (Seq.truncate 5 >> Array.ofSeq) (* take at most 5...  - Array.truncate TBD *) 
            if Seq.length checkMembers > 2 && dominant (Seq.collect (snd >> fst) checkMembers) (Seq.collect (snd >> snd) checkMembers) then
              let! allMembers = nestedMembers id
              let props = allMembers |> Array.map (fun (p, (_, filtered)) ->
                p, filtered)
              return Some(NestedChoice(catDoc, valDoc, catName, valName, props))
            else return None
        | _ -> return None
    | _ -> return None })

type ItemListSchema = { name : string }
type CreateActionSchema = { result : ItemListSchema }
type AddActionSchema = { targetCollection : ItemListSchema }

let collectItemListEditors =
  pickChainSuffixes (fun chain -> async {
    match chain with
    | (caParentTy, caName, Some caSch, caTy, catDoc)::addActions when caSch.Type = "CreateAction" ->
        let listName = (unbox<CreateActionSchema> caSch.JSON).result.name

        /// Collect all AddActions in the rest of the chain and return
        /// the added options together with the type of the last member 
        let rec collectAdds added lastTy = function
          | (addParentTy, addName, Some addSch, addTy, _)::addActions when 
                addSch.Type = "AddAction" &&
                listName = (unbox<AddActionSchema> addSch.JSON).targetCollection.name ->
              collectAdds (addName::added) addTy addActions
          | _ -> List.rev added, lastTy

        // Collect add actions from the rest of the chain & get available
        // actions of the last type in the chain (that can be added)
        let adds, lastTy = collectAdds [] caTy addActions
        let! members = getMembers lastTy
        let availableAdds = members |> filterProperties (function
          | (n, Some s, t) when s.Type = "AddAction" -> (unbox<AddActionSchema> s.JSON).targetCollection.name = listName
          | _ -> false )
        return Some(CreateList(catDoc, caName, Array.ofList adds, availableAdds))
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
      //let nested = []
      return single @ nested @ itemList  }  


// ------------------------------------------------------------------------------------------------
// Editors user interface
// ------------------------------------------------------------------------------------------------

open TheGamma.Html
open TheGamma.TypeChecker

let replace (rng:Range) newValue (text:string) = 
  text.Substring(0, rng.Start) + newValue + text.Substring(rng.End)

let replaceNameWithValue (text:string) (n:Name) value =
  let newValue = escapeIdent value
  replace n.Range newValue text

/// Replace the second string first, assuming it is later in the text
let replaceTwoNamesWithValues (text:string) (n1:Name, n2:Name) (s1, s2) =
  replace n1.Range (escapeIdent s1) (replace n2.Range (escapeIdent s2) text) 
  
let removeRangeWithPrecendingDot (text:string) (rng:Range) = 
  // Once we have comments, we need to skip over them too
  let mutable start = rng.Start
  while start > 0 && text.[start] <> '.'  do start <- start - 1
  text.Substring(0, start) + text.Substring(rng.End)
  
let insertDotTextAfter (origText:string) (rng:Range) ins =
  origText.Substring(0, rng.End) + "." + escapeIdent ins + origText.Substring(rng.End)

let renderDoc = function
  | Documentation.Text(s) -> h?h3 [] [ text s ]
  | Documentation.None -> h?span [] []
  | Documentation.Details(title, details) -> 
      h?div [] [ 
        h?h3 [] [ text title ]
        h?p [] [ text details ] ]

let renderNestedDoc = function
  | Documentation.Details(_, d1), Documentation.Details(t2, d2) ->
      h?h3 [] [ text t2 ], h?p [] [ text d1; text d2 ]
  | _ ->
      h?h3 [] [ text "Choose a value" ], h?p [] [text "First choose a category, then choose a value."]
    
let renderEditor typeCheck (setValue:string -> string -> string -> unit) origText = function
  | SingleChoice(doc, n, ms) ->
      h?div ["class" => "ed-single"] [
        renderDoc doc
        h?div ["class" => "control"] [ 
          h?select 
            [ "change" =!> fun el e -> 
                let value = (unbox<Browser.HTMLSelectElement> el).value
                replaceNameWithValue origText n value |> setValue "single" value ] 
            [ for (Property.Property(name, _, _)) in ms ->
                let sel = if name = n.Name then ["selected" => "selected"] else []
                h?option sel [ text name ] ]
        ]
      ]
  | CreateList(doc, ca, ns, ms) ->
      let edits = ns |> Array.map (fun n -> n, removeRangeWithPrecendingDot origText n.Range)
      let trigger, render = h.part Set.empty (fun s n -> Set.add n s) 
      
      edits |> Array.iter (fun (n, edited) -> 
        async { let! safe = typeCheck edited
                if safe then trigger n.Name } |> Async.StartImmediate)
      
      render <| fun safe ->
        h?div ["class" => "ed-list"] [
          renderDoc doc
          h?div ["class" => "control"] [ 
            h?ul [] [
              for n, edit in edits -> 
                h?li [] [ 
                  let dis = not (Set.contains n.Name safe)
                  yield text n.Name 
                  yield text " "
                  yield h?button [
                    if dis then yield "disabled" => "disabled"
                    yield "click" =!> fun el e -> setValue "list-delete" n.Name edit ] 
                    [ h?i ["class" => if dis then "fa fa-ban" else "fa fa-times" ] [] ]
                ]
            ]
            h?select 
              [ "data-placeholder" => "add another item..."
                "change" =!> fun el e -> 
                  let sel = (el :?> Browser.HTMLSelectElement).value
                  let last = if ns.Length = 0 then ca else ns.[ns.Length - 1]
                  insertDotTextAfter origText last.Range sel |> setValue "list-add" sel
              ] 
              [ yield h?option [] []
                for (Property.Property(name, _, _)) in ms ->
                  h?option [] [ text name ]
              ]
          ]
        ]

  | NestedChoice(doc1, doc2, n1, n2, props) ->
      let update, render = h.part (n1.Name, n2.Name) (fun _ n -> n)
      render <| fun (name1, name2) ->
        let selected = 
          props 
          |> Array.tryFind (fun (Property.Property(name, _, _), nested) -> name = name1) 
          |> Option.map snd
        let nested = defaultArg selected [||]

        let heading, p = renderNestedDoc (doc1, doc2)
        h?div ["class" => "ed-nested"] [
          heading
          p
          h?div ["class" => "control"] [ 
            h?select 
              [ "change" =!> fun el e -> update ((unbox<Browser.HTMLSelectElement> el).value, "") ] 
              [ for (Property.Property(name, _, _), nested) in props ->
                  let sel = if name = name1 then ["selected" => "selected"] else []
                  h?option sel [ text name ] ]
            h?select 
              [ "data-placeholder" => "choose an item..." 
                "change" =!> fun el e -> 
                    let name2 = (unbox<Browser.HTMLSelectElement> el).value
                    replaceTwoNamesWithValues origText (n1, n2) (name1, name2) 
                    |> setValue "nested" name2  ] 
              [ if name2 = "" then yield h?option [] []
                for Property.Property(name, _, _) in nested ->
                  let sel = if name = name2 then ["selected" => "selected"] else []
                  h?option sel [ text name ] ]
          ]
        ]