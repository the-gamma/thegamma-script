module TheGamma.TypeChecker

open TheGamma
open TheGamma.Ast
open TheGamma.Types
open TheGamma.Common
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Type checking 
// ------------------------------------------------------------------------------------------------

type CheckingContext = 
  { Errors : ResizeArray<Error<Range>> 
    Globals : IDictionary<string, Entity> 
    Ranges : IDictionary<Symbol, Range> }

let addError ctx ent err = 
  ctx.Errors.Add(err ctx.Ranges.[ent.Symbol])

let (|FindProperty|_|) (name:Name) (obj:ObjectType) = 
  obj.Members |> Seq.tryPick (function 
    Member.Property(name=n; typ=r; meta=m) when n = name.Name -> Some(m, r) | _ -> None) 

let (|FindMethod|_|) (name:Name) (obj:ObjectType) = 
  obj.Members |> Seq.tryPick (function 
    Member.Method(name=n; arguments=args; meta=m; typ=r) when n = name.Name -> Some (m, args, r) | _ -> None) 

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
  | Type.Object(FindMethod methName (_, args, _)) ->
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
      | Some arg -> name, getType ctx arg, Some arg
      | None when optional -> name, typ, None
      | None ->
          Errors.TypeChecker.parameterMissingValue name |> addError ctx argList
          name, Type.Any, None)

  // Infer assignments for type parameters from actual arguments
  match memTy [ for _, typ, _ in matchedArguments -> typ ] with
  | Some typ -> typ
  | None -> 
      Errors.TypeChecker.parameterConflict |> addError ctx argList
      Type.Any
  

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
  | EntityKind.GlobalValue(name, _) ->
      if not (ctx.Globals.ContainsKey(name.Name)) then
        Errors.TypeChecker.variableNotInScope name.Name |> addError ctx e
        Type.Any
      else
        getType ctx ctx.Globals.[name.Name]

  | EntityKind.Variable(_, inst) ->
      getType ctx inst      

  | EntityKind.ChainElement(true, name, ident, Some inst, _) ->
      match getType ctx inst with 
      | Type.Any -> Type.Any
      | Type.Object(FindProperty name (meta, resTyp)) -> 
          e.Meta <- meta
          resTyp
      | Type.Object obj ->
          Errors.TypeChecker.propertyMissing name.Name obj.Members |> addError ctx ident
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.ChainElement(false, name, ident, Some inst, Some ({ Kind = EntityKind.ArgumentList(ents) } as arglist)) ->
      match getType ctx inst with 
      | Type.Any -> Type.Any
      | Type.Object(FindMethod name (meta, args, resTyp)) ->  
          e.Meta <- meta
          checkMethodCall ctx resTyp args arglist ents
      | Type.Object obj ->
          Errors.TypeChecker.methodMissing name.Name obj.Members |> addError ctx ident
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
      Type.List(typ)

  | EntityKind.Binding(name, { Kind = EntityKind.CallSite(inst, methName, parSpec) }) ->
      // Binding node is used to resolve type of a lambda function variable. 
      // Its antecedent is `EntityKind.CallSite` containing reference to the method around it - 
      // assuming lambda appears in something like: `foo(10, fun x -> ...)`
      match resolveParameterType (getType ctx inst) methName parSpec with
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
  | Type.Object(obj) when topLevel ->
      let! members = obj.Members |> Async.Array.map (fun m -> async {
        match m with
        | Member.Method(n, args, typ, doc, e) -> 
            let! args = args |> Async.map (fun (n, opt, t) -> async {
              let! t = evaluateDelayedType false t
              return n, opt, t }) 
            return Member.Method(n, args, typ, doc, e)
        | prop -> return prop })
      let obj = 
        { new ObjectType with 
            member x.Members = members
            member x.TypeEquals t2 = obj.TypeEquals t2 }
      return Type.Object obj
  | Type.Function(t1s, t2) ->
      let! t2 = evaluateDelayedType topLevel t2
      let! t1s = Async.map (evaluateDelayedType topLevel) t1s
      return Type.Function(t1s, t2)
  | Type.List(t) ->
      let! t = evaluateDelayedType topLevel t
      return Type.List(t)
  | Type.Delayed(f) ->
      let! t = Async.AwaitFuture f
      return! evaluateDelayedType topLevel t
  | t -> return t }


/// Type check entity & return its type. This first recursively processes
/// all antecedants to make sure that no antecedant is delayed  
/// (this way, `getType` can be ordinary synchronouus function)
let typeCheckEntityAsync ctx (e:Entity) = async {
  let visited = Dictionary<Symbol, bool>()

  let rec loop e = async {
    let isGlobal = match e.Kind with EntityKind.GlobalValue _ -> true | _ -> false
    if not (visited.ContainsKey(e.Symbol)) && (isGlobal || e.Type.IsNone) then
      visited.[e.Symbol] <- true
      for a in e.Antecedents do
        do! loop a 
      Log.trace("typechecker", "Type of entity '%s' (%O) is: %O", e.Name, e.Kind, getType ctx e)
      let! t = evaluateDelayedType true (getType ctx e)
      Log.trace("typechecker", "Type of entity '%s' (%O) reduced to: %O", e.Name, e.Kind, t)
      e.Type <- Some t }

  do! loop e
  return getType ctx e }


// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

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

let typeCheckProgram (globals:Entity list) (bound:Binder.BindingResult) prog = async {
  Log.trace("typechecker", "Type checking program")
  try
    let rangeLookup = dict [ for r, e in bound.Entities -> e.Symbol, r ]
    let vars = dict [ for e in globals -> e.Name, e ]
    let ctx = { Globals = vars; Errors = ResizeArray<_>(); Ranges = rangeLookup }
    let! _ = typeCheckEntityAsync ctx prog 
    Log.trace("typechecker", "Completed type checking")
  with e ->
    Log.exn("typechecker", "Type checking program failed: %O", e) }