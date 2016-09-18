#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
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

let findEntity name kind (entities:(Range * Entity)[]) =
  entities |> Seq.find (fun (_, e) -> e.Kind = kind && e.Name.Name = name)

let check (code:string) ename ekind vars = 
  let ctx = Binder.createContext("script")
  let code = code.Replace("\n    ", "\n")
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> globalEntity n t ctx.Root ] 
  let mutable completed = false
  async { do! TypeChecker.typeCheckProgram globals entities prog
          completed <- true } |> Async.StartImmediate
  if not completed then failwith "Asynchronosu operation did not complete"
  let _, ent = entities |> findEntity ename ekind
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

let vars = 
  [ "series", substituteTypes (Map.ofSeq ["a", Type.Any]) (seriesObj ()) 
    "numbers", substituteTypes (Map.ofSeq ["a", Type.Primitive PrimitiveType.Number]) (seriesObj ()) 
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
