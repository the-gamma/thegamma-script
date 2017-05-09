#if INTERACTIVEZ
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Formatter
#endif
open TheGamma
open TheGamma.Ast
open NUnit.Framework

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Tokenize & parse test code
let parse (code:string) = 
  let code = code.Replace("\r", "").Replace("\n    ","\n")
  code, Parser.parseProgram code |> fst

// --------------------------------------------------------------------------------------
// TESTS: Parser keeps all the whitespace
// --------------------------------------------------------------------------------------

[<Test>]
let ``Whitespace preserved in sample call chain (dot at the start)``() =
  let code, prog = parse """
    let z = olympics
      .'by athlete'
        .'United States'.'Michael Phelps'.data
      .'group data'.'by Athlete'.'sum Gold'.then
      .paging.skip(10).take(10)
      .'get series'
        .'with key Athlete'.'and value Gold'"""
  equal code (Ast.formatProgram prog)

[<Test>]
let ``Whitespace preserved in sample call chain (dot at the end)``() =
  let code, prog = parse """
    let z = olympics.
      'by athlete'.
         'United States'.'Michael Phelps'.data.
      'group data'.'by Athlete'.'sum Gold'.then.
      paging.skip(10).take(10).
      'get series'.
        'with key Athlete'.'and value Gold'"""
  equal code (Ast.formatProgram prog)
