module TheGamma.Tests.Formatter

open Fable.Mocha
open TheGamma
open TheGamma.Ast

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

let parse (code:string) =
  let code = code.Replace("\r", "").Replace("\n      ","\n")
  code, Parser.parseProgram code |> fst

// --------------------------------------------------------------------------------------
// TESTS: Parser keeps all the whitespace
// --------------------------------------------------------------------------------------

let tests = testList "Formatter" [
  testCase "Whitespace preserved in sample call chain (dot at the start)" <| fun () ->
    let code, prog = parse """
      let z = olympics
        .'by athlete'
          .'United States'.'Michael Phelps'.data
        .'group data'.'by Athlete'.'sum Gold'.then
        .paging.skip(10).take(10)
        .'get series'
          .'with key Athlete'.'and value Gold'"""
    equal code (Ast.formatProgram prog)

  testCase "Whitespace preserved including placeholder" <| fun () ->
    let code, prog = parse """
      let z = olympics
        .'by athlete'
          .'United States'.'Michael Phelps'.data
        .'group data'.[foo:'by Athlete'].'sum Gold'.then
        .paging.skip(10).take(10)
        .'get series'
          .'with key Athlete'.'and value Gold'"""
    equal code (Ast.formatProgram prog)

  testCase "Whitespace preserved in sample call chain (dot at the end)" <| fun () ->
    let code, prog = parse """
      let z = olympics.
        'by athlete'.
           'United States'.'Michael Phelps'.data.
        'group data'.'by Athlete'.'sum Gold'.then.
        paging.skip(10).take(10).
        'get series'.
          'with key Athlete'.'and value Gold'"""
    equal code (Ast.formatProgram prog)
]
