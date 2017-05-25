#if INTERACTIVE
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#I "../../src/libraries/bin/Debug"
#I "../../src/thegamma/bin/Debug"
#r "libraries.dll"
#r "thegamma.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.PivotTypes
#endif
open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders
open TheGamma.TypeProviders.Pivot
open NUnit.Framework

// ------------------------------------------------------------------------------------------------
// Helper functions for writing assertions (copy & paste from simpletypes.fs)
// ------------------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Assert that type has specified members
let assertMember mem (t, _) = 
  match t with 
  | Type.Object(obj) ->
      let names = set [ for m in obj.Members -> m.Name ]
      if not (names.Contains mem) then equal (String.concat "," names) mem
  | _ -> equal "{ object }" (Ast.formatType t)

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
// Fake series and fake pivot types for tests
// ------------------------------------------------------------------------------------------------

let series =
  { new FSharpProvider.GenericTypeDefinition with
    member x.Members = [| |]
    member x.TypeEquals t = false
    member x.FullName = "series"
    member x.TypeParameterCount = 2
    member x.Apply tys = 
      { new FSharpProvider.GenericTypeSchema with 
          member y.Members = [| |]
          member y.TypeEquals t = false
          member y.TypeDefinition = x
          member y.TypeArguments = tys
          member y.Substitute _ =
            { new FSharpProvider.GenericType with 
                member z.Members = [| |]
                member z.TypeEquals t = false
                member z.TypeArguments = []
                member z.TypeDefinition = x } } }

let types = System.Collections.Generic.Dictionary<_, _>()
let lookupNamed n = types.[n]
types.Add("series", Type.Object series)

let olympics = 
  makePivotGlobalValue "http://demo" "olympics" lookupNamed false
    [ "Athlete", PrimitiveType.String; "Team", PrimitiveType.String
      "Gold", PrimitiveType.Number; "Silver", PrimitiveType.Number ]

let typ = 
  match olympics with
  | TypeProviders.ProvidedType.GlobalValue(_, _, _, typ) -> typ 
  | _ -> failwith "makePivotGlobalValue did not return type"
types.Add("olympics", typ)

let vars = [ "olympics", typ ]

// ------------------------------------------------------------------------------------------------
// Testing the type checker & Pivot type provider
// ------------------------------------------------------------------------------------------------

[<Test>]
let ``Correctly type check simple grouping and aggregation`` () =
  let code = """
    let res = 
      olympics
        .'group data'.'by Athlete'.'sum Gold'.then
        .'get series'
  """
  let actual = check code (isVariable "res") vars
  actual |> assertMember "with key Athlete"
  actual |> assertMember "with key Gold"
  actual |> assertErrors []

[<Test>]
let ``Correctly type check grouping and aggregation with paging and drop`` () =
  let code = """
    let res = 
      olympics
        .'group data'.'by Athlete'.'sum Gold'.'sum Silver'.then
        .paging.skip(10).take(15)
        .'drop columns'.'drop Silver'.then
        .'get series'
  """
  let actual = check code (isVariable "res") vars
  actual |> assertMember "with key Athlete"
  actual |> assertMember "with key Gold"
  actual |> assertErrors []

[<Test>]
let ``Correctly type check grouping and aggregation with placeholder`` () =
  let code = """
    let res = 
      olympics
        .'group data'.'by Athlete'.'sum Gold'.'sum Silver'.then
        .'drop columns'.[drop:'drop Silver'].then
        .'get series'
  """
  let actual = check code (isVariable "res") vars
  actual |> assertMember "with key Athlete"
  actual |> assertMember "with key Gold"
  actual |> assertErrors []