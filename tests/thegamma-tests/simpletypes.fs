module TheGamma.Tests.SimpleTypes

open Fable.Mocha
open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker

// ------------------------------------------------------------------------------------------------
// Helper functions for type-checking code & producing fake types
// ------------------------------------------------------------------------------------------------

let noEmitter = { Emit = fun _ -> failwith "mock emitter" }
let prop n t = { Member.Name = n; Metadata = []; Emitter = noEmitter; Type = t }
let meth n t a = { Member.Name = n; Metadata = []; Emitter = noEmitter; Type = Type.Method(a, fun _ -> t) }
let delay n f = Type.Delayed(Async.CreateNamedFuture n (async { return f () }))
let obj membrs =
  { new ObjectType with
      member x.Members = Array.ofList membrs
      member x.TypeEquals _ = false } |> Type.Object

let str = Type.Primitive PrimitiveType.String
let num = Type.Primitive PrimitiveType.Number
let bool = Type.Primitive PrimitiveType.Bool

let list t = Type.List(t)
let func t1 t2 = Type.Method([{ MethodArgument.Name = ""; Optional = false; Static = false; Type = t1 }], t2)

let isVariable name = function { Kind = EntityKind.Variable(n, _) } when n.Name = name -> true | _ -> false

let check (code:string) cond vars =
  let ctx = Binder.createContext [] "script"
  let code = code.Replace("\n      ", "\n")
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> Interpreter.globalEntity n [] t None ]
  let mutable completed = false
  async { do! TypeChecker.typeCheckProgram globals entities (Interpreter.evaluate globals) prog
          completed <- true } |> Async.StartImmediate
  if not completed then failwith "Asynchronous operation did not complete"
  let _, ent = entities.Entities |> Seq.find (snd >> cond)
  let errors = TypeChecker.collectTypeErrors prog
  ent.Type.Value,
  [ for e in errors -> e.Number, code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1) ]

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

let assertType t1 (t2, _) =
  equal (Ast.formatType t1) (Ast.formatType t2)

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

let typeLevelObj s = delay "typeLevelObj" (fun () ->
  obj [
    prop (s + " (str)") str
    prop (s + " (num)") num
  ])

let typeLevelVar =
  Type.Method
    ( [ { MethodArgument.Name = ""; Type = Type.Primitive PrimitiveType.String; Static = true; Optional = false } ],
      function [_, Some s] -> Some(typeLevelObj (unbox s)) | _ -> None)

let vars = [ "test", testObj (); "typelevel", typeLevelVar ]

// ------------------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------------------

let tests = testList "SimpleTypes" [
  testCase "Type level computation with string argument" <| fun () ->
    let code = """
      let res = typelevel("hello").'hello (num)'
      res"""
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors []

  testCase "Type check chain consisting of property accesses" <| fun () ->
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

  testCase "Report errors for list with elements of mismatching types" <| fun () ->
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

  testCase "Report error when property not found" <| fun () ->
    let code = "let res = test.yadda"
    let actual = check code (isVariable "res") vars
    actual |> assertErrors [303, "yadda"]

  testCase "Report error when method not found" <| fun () ->
    let code = "let res = test.yadda()"
    let actual = check code (isVariable "res") vars
    actual |> assertErrors [303, "yadda"]

  testCase "Report error when instance is not an object" <| fun () ->
    let code = """let res = test.'get the data'.bar"""
    let actual = check code (isVariable "res") vars
    actual |> assertErrors [305, "test.'get the data'"]
]
