module TheGamma.Tests.Main

open Fable.Mocha

let allTests = testList "TheGamma" [
  Compost.tests
  Tokenizer.tests
  Parser.tests
  Formatter.tests
  Binder.tests
  SimpleTypes.tests
  FSharpTypes.tests
  PivotTypes.tests
]

[<EntryPoint>]
let main _ = Mocha.runTests allTests
