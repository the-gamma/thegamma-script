#if INTERACTIVE
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#I "../../src/libraries/bin/Debug"
#I "../../src/thegamma/bin/Debug"
#r "libraries.dll"
#r "thegamma.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.SimpleTypes
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
let delay n f = Type.Delayed(Async.CreateNamedFuture n (async { return f () }))
let obj membrs = 
  { new ObjectType with
      member x.Members = Array.ofList membrs
      member x.TypeEquals _ = false } |> Type.Object

// Helpers for generating primitive types
let str = Type.Primitive PrimitiveType.String
let num = Type.Primitive PrimitiveType.Number
let bool = Type.Primitive PrimitiveType.Bool

// Helper for generating other types
let list t = Type.List(t)
let func t1 t2 = Type.Function([t1], t2)

// Find variable entity
let isVariable name = function { Kind = EntityKind.Variable(n, _) } when n.Name = name -> true | _ -> false

/// Parse and type check given code, find the specified entity & return its type
/// together with all the errors produced by type checking of the program
let check (code:string) cond vars = 
  let ctx = Binder.createContext [] "script"
  let code = code.Replace("\n    ", "\n")
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> Interpreter.globalEntity n [] t None ] 
  let mutable completed = false
  async { do! TypeChecker.typeCheckProgram globals entities prog
          completed <- true } |> Async.StartImmediate
  if not completed then failwith "Asynchronosu operation did not complete"
  let _, ent = entities.Entities |> Seq.find (snd >> cond)
  let errors = TypeChecker.collectTypeErrors prog
  ent.Type.Value,
  [ for e in errors -> e.Number, code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1) ]

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

// ------------------------------------------------------------------------------------------------
// Fake types for tests
// ------------------------------------------------------------------------------------------------

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

let vars = [ "test", testObj () ]

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