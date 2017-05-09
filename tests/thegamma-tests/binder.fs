#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/libraries.dll"
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Binder
#endif
open TheGamma
open TheGamma.Common
open NUnit.Framework

// --------------------------------------------------------------------------------------
// Binder tests
// --------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Check that nth commands of programs have equal/not equal entities
let checkCommands eq n (p1:Program) (p2:Program) = 
  equal eq (p1.Body.Node.[n].Entity.Value.Symbol = p2.Body.Node.[n].Entity.Value.Symbol)

/// Parse sample that's indented with 2 spaces
let parse (code:string) = 
  let code = code.Replace("\n  ", "\n").TrimStart()
  code, code |> Parser.parseProgram |> fst

/// Olympic sample code (in process of writing if `not completed`)
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

/// Disable all logging when running tests
do Log.setEnabled(set [])

[<Test>]
let ``Binder reuses entities when binding twice`` () =
  let ctx = Binder.createContext [] "script1"
  let _, p1 = parse (olympicSample true)
  let e1 = Binder.bindProgram ctx p1
  let c1 = ctx.Table |> ListDictionary.count
  let _, p2 = parse (olympicSample true)
  let e2 = Binder.bindProgram ctx p2
  let c2 = ctx.Table |> ListDictionary.count
  equal c1 c2 

[<Test>]
let ``Binder reuses some entities when one member is changed`` () =
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

[<Test>]
let ``Binder binds all names in a sample program`` () =
  let ctx = Binder.createContext [] "script1"
  let code, p1 = parse (olympicSample true)
  let _, e1 = Binder.bindProgram ctx p1
  let bound = [ for rng, e in e1.Entities -> code.Substring(rng.Start, rng.End - rng.Start + 1) ]
  let names = 
    [ "data1"; "olympics"; "data"; "'group data'"; "'by Athlete'"; "'sum Gold'"; 
      "'sort data'"; "'by Gold descending'"; "paging"; "take"; "10"; "then"
      "data2"; "'get the data'"; "table"; "create"; "set"; "title"; "\"yadda\"" ]
  equal true (Set.isSubset (set names) (set bound))