#if INTERACTIVE
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#I "../../src/libraries/bin/Debug"
#I "../../src/thegamma/bin/Debug"
#r "libraries.dll"
#r "thegamma.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.FSharpTypes
#endif
open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders.FSharpProvider
open NUnit.Framework
(*
// ------------------------------------------------------------------------------------------------
// Helper functions for writing assertions (copy & paste from simpletypes.fs)
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
types.Add("table", importProvidedType "http://demo" lookupNamed table)
types.Add("series", importProvidedType "http://demo" lookupNamed series)

let fapply n typ =
  match lookupNamed n with
  | Type.Object (:? GenericTypeDefinition as gtd) -> 
      Type.Object(gtd.Apply([TypeSchema.Primitive typ]).Substitute(fun _ -> None))
  | _ -> failwith "expected GenericTypeDefinition as named type"

let vars = 
  [ "series", fapply "series" Type.Any
    "numbers", fapply "series" (Type.Primitive PrimitiveType.Number)
    "table", fapply "table" (Type.Any) ]

// ------------------------------------------------------------------------------------------------
// Testing the type checker & F# type provider
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Type check method call and infer result type from argument`` () =
  let code = """
    let res = series.make([1,2,3]).head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call chain and infer result type from argument`` () =
  let code = """
    let res = series.make([1,2,3])
      .add(4).sort(reverse=true).head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call and infer result type from object argument`` () =
  let code = """
    let res = table.make(numbers).data().head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors []

[<Test>]
let ``Type check method call with function as an argument`` () =
  let code = """
    let res = series.make([1,2,3]).map(fun x -> true).head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Bool)
  actual |> assertErrors []

[<Test>]
let ``Report error when name based param is not last`` () =
  let code = """
    let res = numbers.sort(fast=true, false).head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [307,"false"]

[<Test>]
let ``Report error when required parameter is missing value`` () =
  let code = """
    let res = numbers.add().head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertType (Type.Primitive PrimitiveType.Number)
  actual |> assertErrors [308,"()"]

[<Test>]
let ``Report error when method parameter is given a wrong value`` () =
  let code = """
    let res = numbers.add("yo").head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [308,"(\"yo\")"]

[<Test>]
let ``Report error when generic method type cannot be inferred`` () =
  let code = """
    let res = series.makeTwo(1, true).head()
  """
  let actual = check code (isVariable "res") vars
  actual |> assertErrors [308,"(1, true)"]

  *)