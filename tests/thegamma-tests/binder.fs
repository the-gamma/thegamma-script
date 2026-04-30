module TheGamma.Tests.Binder

open Fable.Mocha
open TheGamma
open TheGamma.Common

// --------------------------------------------------------------------------------------
// Binder tests
// --------------------------------------------------------------------------------------

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

let checkCommands eq n (p1:Program) (p2:Program) =
  equal eq (p1.Body.Node.[n].Entity.Value.Symbol = p2.Body.Node.[n].Entity.Value.Symbol)

let parse (code:string) =
  let code = code.Replace("\n  ", "\n").TrimStart()
  code, code |> Parser.parseProgram |> fst

let olympicSample complete =
  """
  let data1 =
    olympics.data
      .'group data'.'by Athlete'.'sum Gold'.then
      .'sort data'.'by Gold descending'
      .then.paging.take(10)
  """
  + ( if complete then "let data2 = data1.'get the data'"
      else "let data2 = data1.'get th" )
  + """
  table.create(data2).set(title="yadda")"""

do Log.setEnabled(set [])

let tests = testList "Binder" [
  testCase "Binder reuses entities when binding twice" <| fun () ->
    let ctx = Binder.createContext [] "script1"
    let _, p1 = parse (olympicSample true)
    let e1 = Binder.bindProgram ctx p1
    let c1 = ctx.Table |> ListDictionary.count
    let _, p2 = parse (olympicSample true)
    let e2 = Binder.bindProgram ctx p2
    let c2 = ctx.Table |> ListDictionary.count
    equal c1 c2

  testCase "Binder reuses some entities when one member is changed" <| fun () ->
    let ctx = Binder.createContext [] "script1"
    let _, p1 = parse (olympicSample false)
    let e1 = Binder.bindProgram ctx p1
    let c1 = ctx.Table |> ListDictionary.count
    let _, p2 = parse (olympicSample true)
    let e2 = Binder.bindProgram ctx p2
    let c2 = ctx.Table |> ListDictionary.count
    equal true (c2 > c1 && c2 < c1*2)
    checkCommands true 0 p1 p2
    checkCommands false 1 p1 p2
    checkCommands false 2 p1 p2

  testCase "Binder binds all names in a sample program" <| fun () ->
    let ctx = Binder.createContext [] "script1"
    let code, p1 = parse (olympicSample true)
    let _, e1 = Binder.bindProgram ctx p1
    let bound = [ for rng, e in e1.Entities -> code.Substring(rng.Start, rng.End - rng.Start + 1) ]
    let names =
      [ "data1"; "olympics"; "data"; "'group data'"; "'by Athlete'"; "'sum Gold'";
        "'sort data'"; "'by Gold descending'"; "paging"; "take"; "10"; "then"
        "data2"; "'get the data'"; "table"; "create"; "set"; "title"; "\"yadda\"" ]
    equal true (Set.isSubset (set names) (set bound))
]
