module TheGamma.Tests.FSharpTypes

open Fable.Mocha
open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders.FSharpProvider

// ------------------------------------------------------------------------------------------------
// Helper functions
// ------------------------------------------------------------------------------------------------

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

let assertType t1 (t2, _) =
  equal (Ast.formatType t1) (Ast.formatType t2)

let assertErrors expectErrors (_, errs) =
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, ac) in List.zip expectErrors errs do
    equal en an
    equal ec ac

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

// ------------------------------------------------------------------------------------------------
// Fake provided types for tests
// ------------------------------------------------------------------------------------------------

let fpar n = { GenericParameterType.kind = "parameter"; name = n } :> AnyType
let flist t =  { ArrayType.kind = "array"; element = t } :> AnyType
let fnum = { PrimitiveType.kind = "primitive"; name = "int" } :> AnyType
let fbool = { PrimitiveType.kind = "primitive"; name = "bool" } :> AnyType
let ffunc a b = { FunctionType.kind = "function"; arguments = [| a |]; returns = b } :> AnyType

let fmeth n tya rty args =
  let args = args |> Array.ofList |> Array.map (fun (n, o, t) ->
    { name = n; optional = o; ``type`` = t })
  { MethodMember.kind = "method"; name = n; typepars = Array.ofList tya
    arguments = args; returns = rty }
let ftyp n tya mems =
  { ExportedType.name = n; typepars = Array.ofList tya;
    ``static`` = false; instance = [| |]; members = Array.ofList mems }
let fnamed n tya =
  { NamedType.kind = "named"; name = n; typargs = Array.ofList tya }

let pair =
  ftyp "pair" [fpar "a"; fpar "b"] [
    fmeth "first" [] (fpar "a") []
    fmeth "second" [] (fpar "b") []
  ]

let series =
  ftyp "series" [fpar "a"] [
    fmeth "make" [fpar "b"] (fnamed "series" [fpar "b"]) [
      "vals", false, flist (fpar "b") ]
    fmeth "makeTwo" [fpar "b"] (fnamed "series" [fpar "b"]) [
      "val1", false, fpar "b"
      "val2", false, fpar "b" ]
    fmeth "range" [] (fnamed "series" [fnum]) [
      "count", false, fnum ]
    fmeth "add" [] (fnamed "series" [fpar "a"]) [
      "val", false, fpar "a" ]
    fmeth "map" [fpar "b"] (fnamed "series" [fpar "b"]) [
      "f", false, (ffunc (fpar "a") (fpar "b")) ]
    fmeth "sort" [] (fnamed "series" [fpar "a"]) [
      "fast", true, fbool
      "reverse", true, fbool ]
    fmeth "join" [fpar "b"] (fnamed "series" [fnamed "pair" [fpar "a"; fpar "b"]]) [
      "s", false, fnamed "series" [fpar "b"] ]
    fmeth "head" [] (fpar "a") []
  ]

let table =
  ftyp "table" [fpar "a"] [
    fmeth "make" [fpar "b"] (fnamed "table" [fpar "b"]) [
      "data", false, fnamed "series" [fpar "b"] ]
    fmeth "data" [] (fnamed "series" [fpar "a"]) []
  ]

let types = System.Collections.Generic.Dictionary<_, _>()
let lookupNamed n = types.[n]
do types.Add("pair", importProvidedType "http://demo" lookupNamed pair)
do types.Add("table", importProvidedType "http://demo" lookupNamed table)
do types.Add("series", importProvidedType "http://demo" lookupNamed series)

let fapply n typ =
  match lookupNamed n with
  | Type.Object obj when isGenericTypeDefinition (box obj) ->
      let gtd = unbox<GenericTypeDefinition> (box obj)
      Type.Object(gtd.Apply([TypeSchema.Primitive typ]).Substitute(fun _ -> None))
  | _ -> failwith "expected GenericTypeDefinition as named type"

let vars =
  [ "series", fapply "series" Type.Any
    "numbers", fapply "series" (Type.Primitive PrimitiveType.Number)
    "table", fapply "table" (Type.Any) ]

// ------------------------------------------------------------------------------------------------
// Tests
// ------------------------------------------------------------------------------------------------

let tests = testList "FSharpTypes" [
  testCase "Type check method call and infer result type from argument" <| fun () ->
    let code = """
      let res = series.make([1,2,3]).head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors []

  testCase "Type check method call chain and infer result type from argument" <| fun () ->
    let code = """
      let res = series.make([1,2,3])
        .add(4).sort(reverse=true).head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors []

  testCase "Type check method call and infer result type from object argument" <| fun () ->
    let code = """
      let res = table.make(numbers).data().head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors []

  testCase "Type check method call with function as an argument" <| fun () ->
    let code = """
      let res = series.make([1,2,3]).map(fun x -> true).head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Bool)
    actual |> assertErrors []

  testCase "Report error when name based param is not last" <| fun () ->
    let code = """
      let res = numbers.sort(fast=true, false).head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors [307,"false"]

  testCase "Report error when required parameter is missing value" <| fun () ->
    let code = """
      let res = numbers.add().head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors [308,"()"]

  testCase "Report error when method parameter is given a wrong value" <| fun () ->
    let code = """
      let res = numbers.add("yo").head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertErrors [310,"(\"yo\")"]

  testCase "Report error when generic method type cannot be inferred" <| fun () ->
    let code = """
      let res = series.makeTwo(1, true).head()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertErrors [310,"(1, true)"]

  testCase "Type check call to join method that returns a pair" <| fun () ->
    let code = """
      let res = series.make([1]).join(series.make(["a"])).head().first()
    """
    let actual = check code (isVariable "res") vars
    actual |> assertType (Type.Primitive PrimitiveType.Number)
    actual |> assertErrors []
]
