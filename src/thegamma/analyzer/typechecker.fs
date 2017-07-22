// ------------------------------------------------------------------------------------------------
// Type checker sets the Type properties of the Entities created by the Binder
// ------------------------------------------------------------------------------------------------
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
    Ranges : IDictionary<Symbol, Range>
    Evaluate : Entity -> EntityValue option }

let addError ctx ent err = 
  ctx.Errors.Add(err ctx.Ranges.[ent.Symbol])

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
let resolveParameterType instTy parSpec = 
  match instTy with
  | Type.Method(args, _) -> 
      let par = 
        match parSpec with
        | Choice1Of2 name -> args |> Seq.tryFind (fun ma -> ma.Name = name)
        | Choice2Of2 idx -> args |> Seq.tryItem idx  
      match par with
      | Some ma -> ma.Type
      | _ -> failwith "resolveParameterType: Parameter specification was incorrect"
  | _ -> failwith "resolveParameterType: Instance is not an object"

/// Check method call - methodName is for logging only; parameterTypes and resultTypeFunc
/// are the type information from `Type.Method` of the parent; `argList` and `args` are the
/// actual type-checked arguments (argList is for storing errors only)
let rec checkMethodCallAsync (methodName:string) ctx (parameterTypes:MethodArgument list) 
    (resultTypeFunc:((Type * RuntimeValue option) list -> Type option)) argList args = async {

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
    parameterTypes |> List.mapi (fun index ma ->
      let arg = 
        if index < positionBased.Length then Some(positionBased.[index]) 
        else Map.tryFind ma.Name nameBased 
      match arg with
      | Some arg -> getType ctx arg, if ma.Static then Some arg else None
      | None when ma.Optional -> ma.Type, None
      | None ->
          Errors.TypeChecker.parameterMissingValue ma.Name |> addError ctx argList
          Type.Any, None)

  // Evalaute arguments of static parameters
  Log.trace("typechecker", "Evaluating arguments of type-level method '%s'", methodName)
  for e in matchedArguments |> Seq.choose snd do e.Value <- ctx.Evaluate e
  Log.trace("typechecker", "Evaluated arguments of '%s': %O", methodName, [| for e in Seq.choose snd matchedArguments -> e.Value |])
  
  let tcargs = matchedArguments |> List.map (function (t, Some e) -> t, Some(e.Value.Value.Value) | (t, _) -> t, None)
  match resultTypeFunc tcargs with
  | Some typ -> return typ
  | None ->   
      Log.trace("typechecker", "Invalid argument type when calling '%s'. Argument types: %O", 
        methodName, (Array.ofList (List.map (fst >> Ast.formatType) matchedArguments)))
      Errors.TypeChecker.parameterConflict |> addError ctx argList
      return Type.Any }
  

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

  // Type check global value reference (from globals) and variable reference (from antecedent)
  | EntityKind.GlobalValue(name, _) ->
      if not (ctx.Globals.ContainsKey(name.Name)) then
        Errors.TypeChecker.variableNotInScope name.Name |> addError ctx e
        Type.Any
      else
        getType ctx ctx.Globals.[name.Name]

  | EntityKind.Variable(_, inst) ->
      getType ctx inst      

  // Member access gets type of a given member, call assumes the called thing was a method
  | EntityKind.Member(inst, nameEnt & { Kind = EntityKind.MemberName name }) ->
      match getType ctx inst with 
      | Type.Any -> Type.Any
      | Type.Object(FindMember name mem) -> 
          e.Meta <- mem.Metadata
          mem.Type
      | Type.Object obj ->
          Errors.TypeChecker.memberMissing name.Name obj.Members |> addError ctx nameEnt
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.MemberAccess(mem) ->
      getType ctx mem     

  | EntityKind.Member(inst, _) ->
      Log.error("typechecker", "typeCheckEntity: Member access is missing member name!")
      failwith "typeCheckEntity: Member access is missing member name!"

  | EntityKind.Call(inst, { Kind = EntityKind.ArgumentList(ents) }) ->
      Log.error("typechecker", "typeCheckEntity: Call to %s has not been type-checked in typeCheckEntityAsync!", (lastChainElement inst).Name)
      failwithf "typeCheckEntity: Call to %s has not been type-checked in typeCheckEntityAsync!" (lastChainElement inst).Name

  | EntityKind.Call(inst, _) ->
      Log.error("typechecker", "typeCheckEntity: Call to %s is missing argument list!", (lastChainElement inst).Name)
      failwithf "typeCheckEntity: Call to %s is missing argument list!" (lastChainElement inst).Name

  // Type of placeholder is the type of its body
  | EntityKind.Placeholder(_, body) ->      
      getType ctx body

  // Operators and lists depend on the types of operands and elements...
  | EntityKind.Operator(l, operator, r) ->      
      let operandTypes = 
        ( match operator with
          | Operator.Equals -> [PrimitiveType.Number; PrimitiveType.String; PrimitiveType.Date; PrimitiveType.Bool]
          | Operator.Plus -> [PrimitiveType.Number; PrimitiveType.String]
          | _ -> [PrimitiveType.Number] ) |> List.map Type.Primitive 

      [l; r] |> List.iteri (fun idx operand ->
        let typ = getType ctx operand
        if operandTypes |> List.forall (fun opt -> not (typesEqual typ opt)) then
          Errors.TypeChecker.numericOperatorExpectsNumbers operator idx operandTypes typ |> addError ctx operand )
      match operator with
      | Operator.Equals | Operator.LessThan | Operator.GreaterThan 
      | Operator.LessThanOrEqual | Operator.GreaterThanOrEqual -> Type.Primitive PrimitiveType.Bool      
      | _ -> getType ctx l

  | EntityKind.List(elems) ->      
      let typs = elems |> List.map (getType ctx)
      let typ = inferListType typs 
      for a in elems do 
        let elty = getType ctx a
        if not (typesEqual typ elty) then
          Errors.TypeChecker.listElementTypeDoesNotMatch typ elty |> addError ctx a
      Type.List(typ)

  | EntityKind.Binding(name, { Kind = EntityKind.CallSite(inst, parSpec) }) ->
      // Binding node is used to resolve type of a lambda function variable. 
      // Its antecedent is `EntityKind.CallSite` containing reference to the method around it - 
      // assuming lambda appears in something like: `foo(10, fun x -> ...)`
      match resolveParameterType (getType ctx inst) parSpec with
      | Type.Method([ma], _) -> ma.Type
      | _ -> failwith "typeCheckEntity: Expected parameter of function type"

  | EntityKind.Binding(name, _) ->
      failwithf "typeCheckEntity: Variable binding %s is missing call site!" name.Name

  | EntityKind.Function(var, body) ->
      let resTyp = getType ctx body
      Type.Method([ { MethodArgument.Name = ""; Optional = false; Static = false; Type = getType ctx var }], fun _ -> Some resTyp)

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
  | EntityKind.CallSite _ -> Type.Any
  | EntityKind.Program _ -> Type.Any
  | EntityKind.MemberName _ -> Type.Any


/// Perform type applications & evaluate delayed types
let rec evaluateDelayedType topLevel (t:Type) = async {
  match t with
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

      match e.Kind with
      | EntityKind.Call(inst, { Kind = EntityKind.ArgumentList(ents) } & arglist) ->
          let errorCount = ctx.Errors.Count
          let! typ = 
            match getType ctx inst with 
            | Type.Any -> async.Return Type.Any
            | Type.Method(parameterTypes, resultTypeFunc) ->  
                checkMethodCallAsync inst.Name ctx parameterTypes resultTypeFunc arglist ents
            | typ ->
                let lastName = lastChainElement inst
                Errors.TypeChecker.notAnMethod lastName.Name typ |> addError ctx inst
                async.Return Type.Any
          e.Type <- Some typ
          e.Errors <- [ for i in errorCount .. ctx.Errors.Count - 1 -> ctx.Errors.[i] ]
      | _ -> ()

      let! t = evaluateDelayedType true (getType ctx e)
      Log.trace("typechecker", "Type of entity '%s' (%s) is: %s", e.Name, formatEntityKind e.Kind, formatType t)
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

let typeCheckProgram (globals:Entity list) (bound:Binder.BindingResult) evaluate prog = async {
  Log.trace("typechecker", "Type checking program")
  try
    let rangeLookup = dict [ for r, e in bound.Entities -> e.Symbol, r ]
    let vars = dict [ for e in globals -> e.Name, e ]
    let ctx = { Globals = vars; Errors = ResizeArray<_>(); Ranges = rangeLookup; Evaluate = evaluate }
    let! _ = typeCheckEntityAsync ctx prog 
    Log.trace("typechecker", "Completed type checking")
  with e ->
    Log.exn("typechecker", "Type checking program failed: %O", e) }