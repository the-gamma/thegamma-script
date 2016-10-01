#if INTERACTIVE
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#I "../../src/libraries/bin/Debug"
#I "../../src/thegamma/bin/Debug"
#r "libraries.dll"
#r "thegamma.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.TypeChecker
#endif
open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open NUnit.Framework

// ------------------------------------------------------------------------------------------------
// Helper functions for type-checking code & producing fake types
// ------------------------------------------------------------------------------------------------

// Helpers for generating object types
let noEmitter = { Emit = fun _ -> failwith "mock emitter" }
let prop n t = Member.Property(n, t, [], noEmitter)
let meth n t a = Member.Method(n, a, t, [], noEmitter)
let obj membrs = Type.Object { Members = Array.ofList membrs; }
let delay n f = Type.Delayed(n, Async.CreateNamedFuture n (async { return f () }))

// Helpers for generating primitive types
let str = Type.Primitive PrimitiveType.String
let num = Type.Primitive PrimitiveType.Number
let bool = Type.Primitive PrimitiveType.Bool

// Helper for generating other types
let forall n t = Type.Forall([n], t)
let apply t f = Type.App(f, [t])
let par n = Type.Parameter(n)
let list t = Type.List(t)
let func t1 t2 = Type.Function([t1], t2)

// Find variable entity
let isVariable name = function { Kind = EntityKind.Variable(n, _) } when n.Name = name -> true | _ -> false

/// Parse and type check given code, find the specified entity & return its type
/// together with all the errors produced by type checking of the program
let check (code:string) cond vars = 
  let ctx = Binder.createContext("script")
  let code = code.Replace("\n    ", "\n")
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> Interpreter.globalEntity n t None ] 
  let mutable completed = false
  async { do! TypeChecker.typeCheckProgram globals entities prog
          completed <- true } |> Async.StartImmediate
  if not completed then failwith "Asynchronosu operation did not complete"
  let _, ent = entities.Entities |> Seq.find (snd >> cond)
  let errors = TypeChecker.collectTypeErrors prog
  ent.Type.Value,
  [ for e in errors -> e.Number, code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1) ]


// ------------------------------------------------------------------------------------------------
// Assertion helpers & fake types for tests
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

let rec tableObj () =
  obj [
    meth "make" (forall "b" (table (par "b"))) [
      "data", false, series (par "b") ]
    prop "data" (series (par "a"))
  ]

and table t = apply t (forall "a" (delay "table" (fun () ->
  tableObj () )))

let vars = 
  [ "series", series Type.Any
    "numbers", series (Type.Primitive PrimitiveType.Number)
    "table", table (Type.Any)
    "test", testObj () ]


// ------------------------------------------------------------------------------------------------
// Testing the type checker
// ------------------------------------------------------------------------------------------------

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
  let actual = check code (isVariable "res") vars
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
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.List (Type.Primitive PrimitiveType.Number))
  actual |> assertErrors [306, "test.'group data'.three.then"; 306, "\"evil\"" ]

[<Test>]
let ``Type check method call and infer result type from argument`` () =
  let code = """
    let res = series.make([1,2,3])
      .add(4).sort(reverse=true).head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call and infer result type from object argument`` () =
  let code = """
    let res = table.make(numbers).data.head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call with function as an argument`` () =
  let code = """
    let res = series.make([1,2,3]).map(fun x -> true).head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Bool)
  actual |> assertErrors []

[<Test>]
let ``Report error when property not found`` () = 
  let code = "let res = test.yadda"
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [303, "yadda"]

[<Test>]
let ``Report error when method not found`` () = 
  let code = "let res = test.yadda()"
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [304, "yadda"]

[<Test>]
let ``Report error when instance is not an object`` () = 
  let code = """let res = test.'get the data'.bar"""
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [305, "test.'get the data'"]

[<Test>]
let ``Report error when name based param is not last`` () =
  let code = """
    let res = numbers.sort(fast=true, false).head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [307,"false"]

[<Test>]
let ``Report error when required parameter is missing value`` () =
  let code = """
    let res = numbers.add().head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [308,"()"]

[<Test>]
let ``Report error when method parameter is given a wrong value`` () =
  let code = """
    let res = numbers.add("yo").head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [309,"\"yo\""]

[<Test>]
let ``Report error when generic method type cannot be inferred`` () =
  let code = """
    let res = series.makeTwo(1, true).head
  """
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [310,"(1, true)"]
