module TheGamma.TypeChecker

open TheGamma
open TheGamma.Ast
open TheGamma.Common
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Helper functions for type equality and substitution
// ------------------------------------------------------------------------------------------------

type TypeContext = 
  { EquivalentVars : (TypeVar * TypeVar) list }

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

let rec memberNamesEqual m1 m2 =
  match m1, m2 with 
  | Member.Property(name=n1), Member.Property(name=n2)
  | Member.Method(name=n1), Member.Method(name=n2) -> n1 = n2 
  | _ -> false

let (|BoundTypeVariables|) t = 
  match t with 
  | Type.Forall(vars, _) -> vars, t
  | _ -> [], t

let rec membersEqual ctx m1 m2 =
  match m1, m2 with 
  | Member.Property(n1, t1, s1, d1, _), Member.Property(n2, t2, s2, d2, _) -> 
      n1 = n2 && s1 = s2 && d1 = d2 && typesEqualAux ctx t1 t2
  | Member.Method(n1, a1, BoundTypeVariables (v1, r1), d1, _), Member.Method(n2, a2, BoundTypeVariables (v2, r2), d2, _) -> 
      let ctx = { ctx with EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      n1 = n2 && d1 = d2 && typesEqualAux ctx r1 r2 && 
        listsEqual a1 a2 (fun (s1, b1, t1) (s2, b2, t2) -> 
          s1 = s2 && b1 = b2 && typesEqualAux ctx t1 t2)
  | _ -> false

and typesEqualAux ctx t1 t2 = 
  match t1, t2 with
  | Type.Any, _ | _, Type.Any -> true
  | Type.Parameter(p1), Type.Parameter(p2) ->
      ctx.EquivalentVars |> List.exists (fun (l, r) -> l = p1 && r = p2)
  | Type.Delayed(g1, _), Type.Delayed(g2, _) -> g1 = g2
  | Type.List t1, Type.List t2 -> typesEqualAux ctx t1 t2
  | Type.Function(a1, r1), Type.Function(a2, r2) -> 
      listsEqual (r1::a1) (r2::a2) (typesEqualAux ctx)
  | Type.Object(o1), Type.Object(o2) -> arraysEqual o1.Members o2.Members (membersEqual ctx)
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2  
  | Type.Forall(v1, t1), Type.Forall(v2, t2) when List.length v1 = List.length v2 ->
      let ctx = { ctx with EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      typesEqualAux ctx t1 t2
  | Type.App(t1, ts1), Type.App(t2, ts2) when List.length ts1 = List.length ts2 ->
      (t1, t2)::(List.zip ts1 ts2) |> List.forall (fun (t1, t2) -> typesEqualAux ctx t1 t2)
  | _ -> false

/// Returns true when closed types have equivalent structure up to renaming of local type variables
let typesEqual = typesEqualAux { EquivalentVars = [] }

let rec substituteMembers assigns members = 
  members |> Array.map (function
    | Member.Method(n,ars,BoundTypeVariables (vars, t),d,e) -> 
        // Generic methods are encoded as methods with forall return type
        // but we need to avoid substituting in parameters too!
        let assigns = vars |> List.fold (fun assigns var -> Map.remove var assigns) assigns
        let ars = ars |> List.map (fun (n,o,t) -> n, o, substituteTypes assigns t)
        Member.Method(n,ars,substituteTypes assigns t,d,e)
    | Member.Property(n,t,s,d,e) -> Member.Property(n,substituteTypes assigns t,s,d,e))      

/// Substitute types for type variables in a given type
and substituteTypes assigns t =
  match t with
  | Type.Parameter s when Map.containsKey s assigns -> assigns.[s]
  | Type.Parameter _ | Type.Any | Type.Primitive _ -> t
  | Type.Function(ts, t) -> Type.Function(List.map (substituteTypes assigns) ts, substituteTypes assigns t)
  | Type.List(t) -> Type.List(substituteTypes assigns t)
  | Type.Object(o) -> { Members = substituteMembers assigns o.Members } |> Type.Object
  | Type.Delayed(g, f) -> 
      let f = 
        { new Future<Type> with 
            member x.Then(g) =             
              f.Then(fun t -> g (substituteTypes assigns t)) }
      Type.Delayed(g, f)
  | Type.App(t, ts) -> Type.App(substituteTypes assigns t, List.map (substituteTypes assigns) ts)
  | Type.Forall(vars, t) ->
      let assigns = vars |> List.fold (fun assigns var -> Map.remove var assigns) assigns
      Type.Forall(vars, substituteTypes assigns t)

type UnifictionContext = 
  { FreeVars : Set<TypeVar>
    Assignments : (TypeVar * Type) list
    EquivalentVars : (TypeVar * TypeVar) list 
    Errors : (Type * Type) list }

let rec unifyTypesAux ctx ts1 ts2 =
  match ts1, ts2 with
  | Type.Parameter n::ts1, t::ts2 when 
        ( ctx.FreeVars.Contains n &&
          match t with Type.Parameter _ -> false | _ -> true ) ->
      unifyTypesAux { ctx with Assignments = (n, t)::ctx.Assignments } ts1 ts2
  | Type.Function(tis1, to1)::ts1, Type.Function(tis2, to2)::ts2 ->
      unifyTypesAux ctx (to1::tis1 @ ts1) (to2::tis2 @ ts2)
  | Type.Object({ Members = m1 })::ts1, Type.Object({ Members = m2 })::ts2 
      when arraysEqual m1 m2 memberNamesEqual ->
        unifyTypesAux ctx ts1 ts2
  | Type.List(t1)::ts1, Type.List(t2)::ts2 -> 
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.Forall(v1, t1)::ts1, Type.Forall(v2, t2)::ts2 when List.length v1 = List.length v2 ->
      let ctx = { ctx with UnifictionContext.EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.App(t1, ta1)::tb1, Type.App(t2, ta2)::tb2 when List.length ta1 = List.length ta2 ->
      unifyTypesAux ctx (t1::(List.append ta1 tb1)) (t2::(List.append ta2 tb2))
  | t1::ts1, t2::ts2 when typesEqualAux { EquivalentVars = ctx.EquivalentVars } t1 t2 ->
      unifyTypesAux ctx ts1 ts2  
  | t1::ts1, t2::ts2 -> 
      unifyTypesAux { ctx with Errors = (t1, t2)::ctx.Errors } ts1 ts2
  | [], [] -> ctx
  | _ -> failwith "unifyTypesAux: The lists of types had mismatching lengths"

/// Unify a type with given free type variables with a given closed type
/// and return assignments (possibly conflicting) with mismatching types 
let unifyTypes free ts1 ts2 = 
  let ctx = { FreeVars = set free; Assignments = []; EquivalentVars = []; Errors = [] }
  let ctx = unifyTypesAux ctx [ts1] [ts2]
  ctx.Assignments, ctx.Errors

/// Perform type applications 
let rec reduceType t = 
  match t with
  | Type.App(Type.Forall(vars, t), args) ->
      if List.length vars <> List.length args then failwith "reduceType: Invalid type application"
      let t = substituteTypes (Map.ofList (List.zip vars args)) t
      reduceType t 
  | _ -> t

// ------------------------------------------------------------------------------------------------
// Type checking 
// ------------------------------------------------------------------------------------------------

type CheckingContext = 
  { Errors : ResizeArray<Error<Range>> 
    Globals : IDictionary<string, Entity> 
    Ranges : IDictionary<Symbol, Range> }

let addError ctx ent err = 
  ctx.Errors.Add(err ctx.Ranges.[ent.Symbol])

let (|FindProperty|_|) (name:Name) { Members = membs } = 
  membs |> Seq.tryPick (function 
    Member.Property(name=n; typ=r) when n = name.Name -> Some r | _ -> None) 

let (|FindMethod|_|) (name:Name) { Members = membs } = 
  membs |> Seq.tryPick (function 
    Member.Method(name=n; arguments=args; typ=r) when n = name.Name -> Some (args, r) | _ -> None) 

/// Given a list of types, find the most frequent type (using Type.Any as the last resort)
let inferListType typs = 
  typs 
  |> List.filter (function Type.Any -> false | _ -> true)
  |> List.groupWith typesEqual
  |> List.map (fun g -> List.head g, List.length g)
  |> List.append [Type.Any, 0]
  |> List.maxBy snd
  |> fst

/// Resolve type of parameter - parSpec can be Choice1Of2 with 
/// parameter name or Choice2Of2 with parameter index.
let resolveParameterType instTy methName parSpec = 
  match instTy with
  | Type.Object(FindMethod methName (args, _)) ->
      match parSpec with
      | Choice1Of2 name -> args |> Seq.pick (fun (n, _, t) -> if n = name then Some t else None) // TODO: Can crash
      | Choice2Of2 idx -> let _, _, t = args.[idx] in t // TODO: Can crash
  | _ -> failwith "resolveParameterType: Instance is not an object"


let rec checkMethodCall ctx memTy pars argList args = 

  // Split arguments into position & name based and report 
  // error if there is non-named argument after named argument
  let positionBased, nameBased = 
    let pb = args |> List.takeWhile (function { Kind = EntityKind.NamedParam _ } -> false | _ -> true)  
    let nb = args |> List.skipWhile (function { Kind = EntityKind.NamedParam _ } -> false | _ -> true)  
    pb |> Array.ofList,
    nb |> List.choose (fun arg -> 
      match arg.Kind with
      | EntityKind.NamedParam(name, value) -> Some(name.Name, value)
      | _ ->
          Errors.TypeChecker.nameBasedParamMustBeLast |> addError ctx arg
          None ) |> Map.ofList

  // Match actual arguments with the parameters and report
  // error if non-optional parameter is missing an assignment
  let matchedArguments = 
    pars |> List.mapi (fun index (name, optional, typ) ->
      let arg = 
        if index < positionBased.Length then Some(positionBased.[index]) 
        else Map.tryFind name nameBased 
      match arg with
      | Some arg -> name, typ, getType ctx arg, Some arg
      | None when optional -> name, typ, typ, None
      | None ->
          Errors.TypeChecker.parameterMissingValue name |> addError ctx argList
          name, typ, Type.Any, None)

  // Infer assignments for type parameters from actual arguments
  let tyVars, resTy = match memTy with Type.Forall(tya, resTy) -> tya, resTy | resTy -> [], resTy
  let assigns = 
    matchedArguments |> List.collect (fun (name, parTy, argTy, entityOpt) ->
      let assigns, errors = unifyTypes tyVars parTy argTy
      if entityOpt.IsSome then
        for t1, t2 in errors do
          Errors.TypeChecker.incorrectParameterType name parTy argTy t1 t2 |> addError ctx entityOpt.Value 
      assigns )

  // Report errors if we inferred conflicting assignments for one variable
  for _, group in List.groupBy fst assigns do
    match group with
    | (v, t1)::(_::_ as ts) ->
        for _, t in ts do
          Errors.TypeChecker.inferenceConflict v t1 t |> addError ctx argList
    | _ -> ()
  
  // Substitute in the return type
  let res = substituteTypes (Map.ofList assigns) resTy
  //printfn "Result of call: %A" res
  res
  

/// Get type of an entity and record errors generated when type checking this entity
and getType ctx (e:Entity) = 
  if e.Type.IsNone then 
    let errorCount = ctx.Errors.Count
    e.Type <- Some (typeCheckEntity ctx e)
    e.Errors <- [ for i in errorCount .. ctx.Errors.Count - 1 -> ctx.Errors.[i] ]
  e.Type.Value

/// Type check entity - assumes that all antecedents of the entity 
/// have been reduced to non-delayed type before
and typeCheckEntity ctx (e:Entity) = 
  match e.Kind with
  | EntityKind.GlobalValue(name) ->
      if not (ctx.Globals.ContainsKey(name.Name)) then
        Errors.TypeChecker.variableNotInScope name.Name |> addError ctx e
        Type.Any
      else
        getType ctx ctx.Globals.[name.Name]

  | EntityKind.Variable(_, inst) ->
      getType ctx inst      

  | EntityKind.ChainElement(true, name, ident, Some inst, _) ->
      match reduceType (getType ctx inst) with 
      | Type.Any -> Type.Any
      | Type.Object(FindProperty name resTyp) -> resTyp
      | Type.Object { Members = members } ->
          Errors.TypeChecker.propertyMissing name.Name members |> addError ctx ident
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.ChainElement(false, name, ident, Some inst, Some ({ Kind = EntityKind.ArgumentList(ents) } as arglist)) ->
      match reduceType (getType ctx inst) with 
      | Type.Any -> Type.Any
      | Type.Object(FindMethod name (args, resTyp)) -> checkMethodCall ctx resTyp args arglist ents
      | Type.Object { Members = members } ->
          Errors.TypeChecker.methodMissing name.Name members |> addError ctx ident
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.ChainElement(_, name, ident, None, _) ->
      Errors.TypeChecker.callMissingInstance name.Name |> addError ctx ident
      Type.Any

  | EntityKind.ChainElement(false, name, _, _, _) ->
      failwith (sprintf "typeCheckEntity: Call to %s is missing argument list!" name.Name)
      
  | EntityKind.Operator(l, operator, r) ->      
      [l; r] |> List.iteri (fun idx operand ->
        let typ = getType ctx operand 
        if not (typesEqual typ (Type.Primitive PrimitiveType.Number)) then
          Errors.TypeChecker.numericOperatorExpectsNumbers operator idx typ |> addError ctx operand )
      Type.Primitive PrimitiveType.Number

  | EntityKind.List(elems) ->      
      let typs = elems |> List.map (getType ctx)
      let typ = inferListType typs 
      for a in elems do 
        let elty = getType ctx a
        if not (typesEqual typ elty) then
          Errors.TypeChecker.listElementTypeDoesNotMatch typ elty |> addError ctx a
      Type.List typ

  | EntityKind.Binding(name, { Kind = EntityKind.CallSite(inst, methName, parSpec) }) ->
      // Binding node is used to resolve type of a lambda function variable. 
      // Its antecedent is `EntityKind.CallSite` containing reference to the method around it - 
      // assuming lambda appears in something like: `foo(10, fun x -> ...)`
      match resolveParameterType (reduceType (getType ctx inst)) methName parSpec with
      | Type.Function([tin], _) -> tin
      | _ -> failwith "typeCheckEntity: Expected parameter of function type"

  | EntityKind.Binding(name, _) ->
      failwith (sprintf "typeCheckEntity: Variable binding %s is missing call site!" name.Name)

  | EntityKind.Function(var, body) ->
      Type.Function([getType ctx var], getType ctx body)

  // Entities with primitive types
  | EntityKind.Constant(Constant.Number _) -> Type.Primitive(PrimitiveType.Number)
  | EntityKind.Constant(Constant.String _) -> Type.Primitive(PrimitiveType.String)
  | EntityKind.Constant(Constant.Boolean _) -> Type.Primitive(PrimitiveType.Bool)
  | EntityKind.Constant(Constant.Empty) -> Type.Any

  // Entities that do not have a real type
  | EntityKind.Root -> Type.Any
  | EntityKind.LetCommand _ -> Type.Any
  | EntityKind.RunCommand _ -> Type.Any
  | EntityKind.ArgumentList _ -> Type.Any
  | EntityKind.NamedParam _ -> Type.Any
  | EntityKind.NamedMember _ -> Type.Any
  | EntityKind.CallSite _ -> Type.Any
  | EntityKind.Program _ -> Type.Any


/// Perform type applications & evaluate delayed types
let rec evaluateDelayedType topLevel (t:Type) = async {
  match t with
  | Type.App(t, args) ->
      let! t = evaluateDelayedType topLevel t 
      return Type.App(t, args)
  | Type.Forall(vars, t) ->
      let! t = evaluateDelayedType topLevel t 
      return Type.Forall(vars, t)  
  | Type.Object(obj) when topLevel ->
      let! members = obj.Members |> Async.Array.map (fun m -> async {
        match m with
        | Member.Method(n, args, typ, doc, e) -> 
            let! args = args |> Async.map (fun (n, opt, t) -> async {
              let! t = evaluateDelayedType false t
              return n, opt, t }) 
            return Member.Method(n, args, typ, doc, e)
        | prop -> return prop })
      return Type.Object { obj with Members = members }
  | Type.Function(t1s, t2) ->
      let! t2 = evaluateDelayedType topLevel t2
      let! t1s = Async.map (evaluateDelayedType topLevel) t1s
      return Type.Function(t1s, t2)
  | Type.List(t) ->
      let! t = evaluateDelayedType topLevel t
      return Type.List(t)
  | Type.Delayed(_, f) ->
      let! t = Async.AwaitFuture f
      return! evaluateDelayedType topLevel t
  | t -> return t }


/// Type check entity & return its type. This first recursively processes
/// all antecedants to make sure that no antecedant is delayed  
/// (this way, `getType` can be ordinary synchronouus function)
let typeCheckEntityAsync ctx (e:Entity) = async {
  let visited = Dictionary<Symbol, bool>()

  let rec loop e = async {
    if not (visited.ContainsKey(e.Symbol)) && e.Type.IsNone then
      visited.[e.Symbol] <- true
      for a in e.Antecedents do
        do! loop a 
      Log.trace("typechecker", "Checking entity '%s' (%O)", e.Name, e.Kind)
      let! t = evaluateDelayedType true (getType ctx e)
      Log.trace("typechecker", "Type of entity '%s' (%O) is: %O", e.Name, e.Kind, t)
      e.Type <- Some t }

  do! loop e
  return getType ctx e }


// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

let globalEntity name typ root =           
  { Kind = EntityKind.GlobalValue({ Name = name })
    Symbol = createSymbol()
    Type = Some typ 
    Errors = [] }

let collectTypeErrors (entity:Entity) = 
  let errors = ResizeArray<_>()
  let visited = Dictionary<Symbol, bool>()
  let rec loop e = 
    if not (visited.ContainsKey e.Symbol) then
      visited.[e.Symbol] <- true
      for e in e.Antecedents do loop e
      errors.AddRange(e.Errors)
  loop entity
  errors.ToArray()

let typeCheckProgram (globals:Entity list) bound prog = async {
  Log.trace("typechecker", "Type checking program")
  try
    let rangeLookup = dict [ for r, e in bound -> e.Symbol, r ]
    let vars = dict [ for e in globals -> e.Name, e ]
    let ctx = { Globals = vars; Errors = ResizeArray<_>(); Ranges = rangeLookup }
    let! _ = typeCheckEntityAsync ctx prog 
    Log.trace("typechecker", "Completed type checking")
  with e ->
    Log.exn("typechecker", "Type checking program failed: %O", e) }