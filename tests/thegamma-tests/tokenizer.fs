module TheGamma.Tests.Tokenizer

open Fable.Mocha
open TheGamma
open TheGamma.Ast
open TheGamma.Tokenizer

// --------------------------------------------------------------------------------------
// Helpers for writing property tests for tokenizer
// --------------------------------------------------------------------------------------

let rnd = System.Random()

let formatTokensUsingRange (source:string) (tokens:seq<Token>) =
  tokens
  |> Seq.choose (fun t ->
      let len = t.Range.End - t.Range.Start + 1
      if t.Range.Start + len > source.Length then None
      else Some(source.Substring(t.Range.Start, len)))
  |> String.concat ""

let randomString (s:string) =
  Array.init (rnd.Next 10) (fun _ -> s.[rnd.Next(s.Length)]) |> System.String

let constantTokens =
  [ TokenKind.LParen; TokenKind.RParen; TokenKind.Equals; TokenKind.Dot; TokenKind.Comma
    TokenKind.Let; TokenKind.LSquare; TokenKind.RSquare; TokenKind.Fun; TokenKind.Arrow
    TokenKind.Colon; TokenKind.Operator Operator.Divide; TokenKind.Operator Operator.GreaterThan
    TokenKind.Operator Operator.GreaterThanOrEqual; TokenKind.Operator Operator.LessThan
    TokenKind.Operator Operator.LessThanOrEqual; TokenKind.Operator Operator.Minus
    TokenKind.Operator Operator.Multiply; TokenKind.Operator Operator.Plus; TokenKind.Newline
    TokenKind.Boolean true; TokenKind.Boolean false ]

let randomToken last =
  match rnd.Next(12) with
  | 0 | 1 | 2 | 3 when constantTokens |> Seq.exists ((=) last) |> not ->
      match last, constantTokens.[rnd.Next(constantTokens.Length)] with
      | TokenKind.Ident _, TokenKind.Let
      | TokenKind.Ident _, TokenKind.Fun
      | TokenKind.Ident _, TokenKind.Boolean _
      | TokenKind.Number _, TokenKind.Dot -> TokenKind.Arrow
      | _, t -> t
  | 4 when (match last with TokenKind.Ident _ | TokenKind.Number _ -> false | _ -> true) ->
      let n = rnd.Next(0, 1000000000)
      TokenKind.Number(string n, float n)
  | 5 when (match last with TokenKind.Ident _ | TokenKind.Number _ -> false | _ -> true) ->
      let n1, n2 = rnd.Next(0, 1000000), rnd.Next(0, 1000000)
      let n = string n1 + "." + string n2
      TokenKind.Number(n, float n)
  | 6 -> TokenKind.String(randomString "abcDEF012$*^!@#+,. \n\\\"")
  | 7 when (match last with TokenKind.Number _ | TokenKind.Ident _ -> false | _ -> true) ->
      TokenKind.Ident("a" + randomString "bcdEFG0123")
  | 8 -> TokenKind.QIdent(randomString "bcdEFG0123$*^!@#+,. \\\"")
  | _ when (match last with TokenKind.White _ -> false | _ -> true) ->
      TokenKind.White(" " + randomString " ")
  | _ -> TokenKind.Comma

let withString () =
  Array.init 100 (fun _ -> randomString "\"'\\\n abcDEF1234,[].+-/<>=@#$%")
  |> String.concat ""

let withTokens () =
  TokenKind.Dot
  |> Seq.unfold (fun last ->
    let t = { Token = randomToken last; Range = { Start = 0; End = 0 } }
    Some(t, t.Token))
  |> Seq.take (20 + rnd.Next(100))
  |> List.ofSeq

let check g (f:_ -> unit) =
  for i in 1 .. 100 do
    f (g())
  printfn "      (Passed 100 tests)"

let tokensEqual t1 t2 =
  let eq =
    List.zip (List.ofSeq t1) (List.ofSeq t2)
    |> List.forall (fun (t1, t2) -> t1.Token = t2.Token)
  if not eq then failwith "Tokens do not match"

let equal (a:'T) (b:'T) =
  if a <> b then failwithf "Expected '%A' but was '%A'" a b

// --------------------------------------------------------------------------------------
// TESTS: Property tests for tokenizer
// --------------------------------------------------------------------------------------

let tests = testList "Tokenizer" [
  testCase "Format and tokenize returns original tokens" <| fun () ->
    check withTokens (fun sourceTokens ->
      let parsedTokens, errors = Tokenizer.tokenize (formatTokens sourceTokens)
      tokensEqual parsedTokens sourceTokens)

  testCase "Format and tokenize does not produce errors" <| fun () ->
    check withTokens (fun sourceTokens ->
      let parsedTokens, errors = Tokenizer.tokenize (formatTokens sourceTokens)
      equal [||] errors)

  testCase "Concatenate using ranges recreates original input for valid tokens" <| fun () ->
    check withTokens (fun sourceTokens ->
      let input = formatTokens sourceTokens
      let parsedTokens, _ = Tokenizer.tokenize input
      let output = formatTokensUsingRange input parsedTokens
      equal input output)

  testCase "Concatenate using ranges recreates original input for random string" <| fun () ->
    check withString (fun input ->
      let parsedTokens, _ = Tokenizer.tokenize input
      let output = formatTokensUsingRange input parsedTokens
      equal input output)
]
