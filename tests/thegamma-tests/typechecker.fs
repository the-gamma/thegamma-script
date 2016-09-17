#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.TypeChecker
#endif
open TheGamma
open TheGamma.Common
open NUnit.Framework


// ------------------------------------------------------------------------------------------------
// 
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

let rec membersEqual ctx m1 m2 =
  match m1, m2 with 
  | Member.Property(n1, t1, s1, d1, _), Member.Property(n2, t2, s2, d2, _) -> 
      n1 = n2 && s1 = s2 && d1 = d2 && typesEqualAux ctx t1 t2
  | Member.Method(n1, a1, r1, d1, _), Member.Method(n2, a2, r2, d2, _) -> 
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
  | Type.Object(o1), Type.Object(o2) -> 
      arraysEqual o1.Members o2.Members (membersEqual ctx)
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2  
  | Type.Forall(v1, t1), Type.Forall(v2, t2) when List.length v1 = List.length v2 ->
      let ctx = { ctx with EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      typesEqualAux ctx t1 t2
  | Type.App(t1, ts1), Type.App(t2, ts2) when List.length ts1 = List.length ts2 ->
      (t1, t2)::(List.zip ts1 ts2) |> List.forall (fun (t1, t2) -> typesEqualAux ctx t1 t2)
  | _ -> false

/// Returns true when closed types have equivalent structure up to renaming of local type variables
let typesEqual = typesEqualAux { EquivalentVars = [] }

let (|BoundTypeVariables|) t = 
  match t with 
  | Type.Forall(vars, _) -> vars, t
  | _ -> [], t

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
  | t1::ts1, t2::ts2 when typesEqualAux { EquivalentVars = ctx.EquivalentVars } t1 t2 ->
      unifyTypesAux ctx ts1 ts2  
  | Type.Parameter n::ts1, t::ts2 when 
        ( ctx.FreeVars.Contains n &&
          match t with Type.Parameter _ -> false | _ -> true ) ->
      unifyTypesAux { ctx with Assignments = (n, t)::ctx.Assignments } ts1 ts2
  | Type.Function(tis1, to1)::ts1, Type.Function(tis2, to2)::ts2 ->
      unifyTypesAux ctx (to1::tis1 @ ts1) (to2::tis2 @ ts2)
  | Type.List(t1)::ts1, Type.List(t2)::ts2 -> 
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.Forall(v1, t1)::ts1, Type.Forall(v2, t2)::ts2 when List.length v1 = List.length v2 ->
      let ctx = { ctx with UnifictionContext.EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.App(t1, ta1)::tb1, Type.App(t2, ta2)::tb2 when List.length ta1 = List.length tb2 ->
      unifyTypesAux ctx (t1::(List.append ta1 tb1)) (t2::(List.append ta2 tb2))
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

// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

open System.Collections.Generic

type CheckingContext = 
  { Errors : ResizeArray<Error<Range>> 
    Globals : IDictionary<string, Type> 
    Ranges : IDictionary<Symbol, Range> }

let addError ctx ent err = 
  ctx.Errors.Add(err ctx.Ranges.[ent.Symbol])

let (|FindProperty|_|) (name:Name) { Members = membs } = 
  membs |> Seq.tryPick (function Member.Property(name=n; typ=r) when n = name.Name -> Some r | _ -> None) 

let (|FindMethod|_|) (name:Name) { Members = membs } = 
  membs |> Seq.tryPick (function Member.Method(name=n; arguments=args; typ=r) when n = name.Name -> Some (args, r) | _ -> None) 

let inferListType typs = 
  typs 
  |> List.filter (function Type.Any -> false | _ -> true)
  |> List.groupWith typesEqual
  |> List.map (fun g -> List.head g, List.length g)
  |> List.append [Type.Any, 0]
  |> List.maxBy snd
  |> fst

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
    let pb = args |> List.takeWhile (fun arg -> arg.Kind <> EntityKind.NamedParam) 
    let nb = args |> List.skipWhile (fun arg -> arg.Kind <> EntityKind.NamedParam)
    pb |> Array.ofList,
    nb |> List.choose (fun arg -> 
      match arg.Kind with
      | EntityKind.NamedParam -> Some(arg.Name.Name, arg.Antecedents.[0])
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
  

and getType ctx (e:Entity) = 
  if e.Type.IsNone then 
    let errorCount = ctx.Errors.Count
    e.Type <- Some (typeCheckEntity ctx e)
    e.Errors <- ctx.Errors.GetRange(errorCount, ctx.Errors.Count - errorCount) |> List.ofSeq
  e.Type.Value

and typeCheckEntity ctx (e:Entity) = 
  match e.Kind with
  | EntityKind.GlobalValue ->
      if not (ctx.Globals.ContainsKey(e.Name.Name)) then
        Errors.TypeChecker.variableNotInScope e.Name.Name |> addError ctx e
        Type.Any
      else
        ctx.Globals.[e.Name.Name]

  | EntityKind.Variable ->
      let inst = match e.Antecedents with [a] -> a | _ -> failwith "typeCheckEntity: Variable should have one antecedent"
      // TODO: Does not work for lambdas
      getType ctx inst      

  | EntityKind.ChainElement(isProperty = true) ->
      let name, inst = match e.Antecedents with [name; a] -> name, a | _ -> failwith "typeCheckEntity: Property should have two antecedents"
      let typ = getType ctx inst
      match typ with 
      | Type.Any -> Type.Any
      | Type.Object(FindProperty e.Name resTyp) -> resTyp
      | Type.Object { Members = members } ->
          Errors.TypeChecker.propertyMissing e.Name.Name members |> addError ctx name
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject e.Name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.ChainElement(isProperty = false; hasInstance = hasInstance) ->
      if not hasInstance then failwith "typeCheckEntity: Method calls without instance are not supported"
      let name, inst, arglist, ents = 
        match e.Antecedents with 
        | [name; inst; arg] -> name, inst, arg, List.tail arg.Antecedents // arg is ArgumentList entity
        | _ -> failwith "typeCheckEntity: Method call is missing required antecedent"
      printfn "Calling %s with arguments %A" name.Name.Name (ents |> List.map (fun e -> e.Name, e.Kind))
      //printfn "Type of instance of %s(...): %A" e.Name.Name (getType ctx inst)
      match getType ctx inst with 
      | Type.Any -> Type.Any
      | Type.Object(FindMethod e.Name (args, resTyp)) -> checkMethodCall ctx resTyp args arglist ents
      | Type.Object { Members = members } ->
          Errors.TypeChecker.methodMissing e.Name.Name members |> addError ctx name
          Type.Any
      | typ ->
          Errors.TypeChecker.notAnObject e.Name.Name typ |> addError ctx inst
          Type.Any

  | EntityKind.Operator operator ->      
      e.Antecedents |> List.iteri (fun idx operand ->
        let typ = getType ctx operand 
        if not (typesEqual typ (Type.Primitive PrimitiveType.Number)) then
          Errors.TypeChecker.numericOperatorExpectsNumbers operator idx typ |> addError ctx operand )
      Type.Primitive PrimitiveType.Number

  | EntityKind.List ->      
      let typs = e.Antecedents |> List.map (getType ctx)
      let typ = inferListType typs 
      for a in e.Antecedents do 
        let elty = getType ctx a
        if not (typesEqual typ elty) then
          Errors.TypeChecker.listElementTypeDoesNotMatch typ elty |> addError ctx a
      Type.List typ

  | EntityKind.Binding ->
      let inst, methName, parSpec = 
        match e.Antecedents with 
        | [ { Kind=EntityKind.CallSite p; Name = name } as v ] -> List.head v.Antecedents, name, p
        | _ -> failwith "typeCheckEntity: Property should have two antecedents"
      match resolveParameterType (getType ctx inst) methName parSpec with
      | Type.Function([tin], _) -> tin
      | _ -> failwith "typeCheckEntity: Expected parameter of function type"

  | EntityKind.Function ->
      let var, body = match e.Antecedents with [v; b] -> v, b | _ -> failwith "typeCheckEntity: Property should have two antecedents"
      Type.Function([getType ctx var], getType ctx body)

  // Entities with primitive types
  | EntityKind.Constant(Constant.Number _) -> Type.Primitive(PrimitiveType.Number)
  | EntityKind.Constant(Constant.String _) -> Type.Primitive(PrimitiveType.String)
  | EntityKind.Constant(Constant.Boolean _) -> Type.Primitive(PrimitiveType.Bool)
  | EntityKind.Constant(Constant.Empty) -> Type.Any

  // Entities that do not have a real type
  | EntityKind.Root -> Type.Any
  | EntityKind.Command -> Type.Any
  | EntityKind.ArgumentList -> Type.Any
  | EntityKind.NamedParam -> Type.Any
  | EntityKind.NamedMember -> Type.Any
  | EntityKind.CallSite _ -> Type.Any
  | EntityKind.Scope -> Type.Any

let rec reduceType t = async {
  match t with
  | Type.App(Type.Forall(vars, t), args) ->
      if List.length vars <> List.length args then failwith "reduceType: Invalid type application"
      let t = substituteTypes (Map.ofList (List.zip vars args)) t
      return! reduceType t 

  | Type.Delayed(_, f) ->
      let! t = Async.AwaitFuture f
      return! reduceType t
  | _ -> return t }

let rec typeCheckEntityAsync ctx (e:Entity) = async {
  for a in e.Antecedents do
    let! _ = typeCheckEntityAsync ctx a
    ()
  let! t = reduceType (getType ctx e)
  e.Type <- Some t
  return t }
  


// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

let noEmitter = { Emit = fun _ -> failwith "mock emitter" }
let prop n t = Member.Property(n, t, None, Documentation.None, noEmitter)
let meth n t a = Member.Method(n, a, t, Documentation.None, noEmitter)
let obj membrs = Type.Object { Members = Array.ofList membrs }
let str = Type.Primitive PrimitiveType.String
let num = Type.Primitive PrimitiveType.Number
let bool = Type.Primitive PrimitiveType.Bool
let delay n f = Type.Delayed(n, Async.AsFuture n (async { return f () }))
let forall n t = Type.Forall([n], t)
let apply t f = Type.App(f, [t])
let par n = Type.Parameter(n)
let list t = Type.List(t)
let func t1 t2 = Type.Function([t1], t2)

let parse (code:string) = 
  let ctx = Binder.createContext("script")
  let code = code.Replace("\n    ", "\n")
  let prog = code |> Parser.parseProgram |> fst
  code, Binder.bindProgram ctx prog 

let findEntity name kind (entities:(Range * Entity)[]) =
  entities |> Seq.find (fun (_, e) -> e.Kind = kind && e.Name.Name = name)

let check code ename ekind vars = 
  let code, ents = parse code
  let rangeLookup = dict [ for r, e in ents -> e.Symbol, r ]
  let ctx = { Globals = vars; Errors = ResizeArray<_>(); Ranges = rangeLookup }
  let ent = ents |> findEntity ename ekind |> snd
  Async.RunSynchronously (typeCheckEntityAsync ctx ent),
  [ for e in ctx.Errors -> e.Number, code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1) ]


// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Assert that two types are equal
let assertType t1 (t2, _) = 
  equal (Ast.formatType t1) (Ast.formatType t2)

/// Assert that result contains given errors
let assertErrors expectErrors (_, errs) = 
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, ac) in List.zip expectErrors errs do
    equal en an
    equal ec ac

let rec fieldsObj t = delay "fieldsObj" (fun () ->
  obj [
    prop "one" (fieldsObj t)
    prop "two" (fieldsObj t)
    prop "three" (fieldsObj t)
    prop "then" t
  ])

let rec testObj () = delay "testObj" (fun () ->
  obj [
    prop "group data" (fieldsObj (testObj ()))
    prop "sort data" (fieldsObj (testObj ()))
    prop "get the data" num
  ])

  
let rec seriesObj () = 
  obj [
    meth "make" (forall "b" (series (par "b"))) [
      "vals", false, list (par "b") ]
    meth "makeTwo" (forall "b" (series (par "b"))) [
      "val1", false, par "b"
      "val2", false, par "b" ]
    meth "range" (series num) [
      "count", false, num ]
    meth "add" (series (par "a")) [
      "val", false, par "a" ]
    meth "map" (forall "b" (series (par "b"))) [
      "f", false, (func (par "a") (par "b")) ]
    meth "sort" (series (par "a")) [
      "fast", true, bool
      "reverse", true, bool ]
    prop "head" (par "a")
  ]

and series t = apply t (forall "a" (delay "series" (fun () ->
  seriesObj () )))

let vars = 
  [ "series", substituteTypes (Map.ofSeq ["a", Type.Any]) (seriesObj ()) 
    "numbers", substituteTypes (Map.ofSeq ["a", Type.Primitive PrimitiveType.Number]) (seriesObj ()) 
    "test", testObj () ] |> dict

[<Test>]
let ``Type check chain consisting of property accesses`` () =
  let code = """
    let res = test.
      'group data'
        .one.two.three.then.
      'sort data'
        .three.two.one.then.
      'get the data'
    res"""
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Report errors for list with elements of mismatching types`` () =
  let code = """
    let res = [ 
      test.'group data'.one.then.'get the data',
      42,
      test.'group data'.two.then.'get the data',
      test.'group data'.three.then,
      "evil" ]
    """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.List (Type.Primitive PrimitiveType.Number))
  actual |> assertErrors [306, "test.'group data'.three.then"; 306, "\"evil\"" ]

[<Test>]
let ``Type check method call and infer result type from argument`` () =
  let code = """
    let res = series.make([1,2,3])
      .add(4).sort(reverse=true).head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call with function as an argument`` () =
  let code = """
    let res = series.make([1,2,3]).map(fun x -> true).head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Bool)
  actual |> assertErrors []

[<Test>]
let ``Report error when property not found`` () = 
  let code = "let res = test.yadda"
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertErrors [303, "yadda"]

[<Test>]
let ``Report error when method not found`` () = 
  let code = "let res = test.yadda()"
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertErrors [304, "yadda"]

[<Test>]
let ``Report error when instance is not an object`` () = 
  let code = """let res = test.'get the data'.bar"""
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertErrors [305, "test.'get the data'"]

[<Test>]
let ``Report error when name based param is not last`` () =
  let code = """
    let res = numbers.sort(fast=true, false).head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [307,"false"]

[<Test>]
let ``Report error when required parameter is missing value`` () =
  let code = """
    let res = numbers.add().head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [308,"()"]

[<Test>]
let ``Report error when method parameter is given a wrong value`` () =
  let code = """
    let res = numbers.add("yo").head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [309,"\"yo\""]

[<Test>]
let ``Report error when generic method type cannot be inferred`` () =
  let code = """
    let res = series.makeTwo(1, true).head
  """
  let actual = check code "res" EntityKind.Variable vars
  actual |> assertErrors [310,"(1, true)"]
