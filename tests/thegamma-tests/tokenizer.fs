#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Tokenizer
#endif
open TheGamma
open TheGamma.Tokenizer
open NUnit.Framework

// --------------------------------------------------------------------------------------
// Helpers for writing property tests for tokenizer
// --------------------------------------------------------------------------------------

/// Global random number generator
let rnd = System.Random()

/// Format a single token
let formatToken = function
  | TokenKind.LParen -> "("
  | TokenKind.RParen -> ")"
  | TokenKind.Equals -> "="
  | TokenKind.Dot -> "."
  | TokenKind.Comma -> ","
  | TokenKind.Let -> "let"
  | TokenKind.LSquare -> "["
  | TokenKind.RSquare -> "]"
  | TokenKind.Fun -> "fun"
  | TokenKind.Arrow -> "->"
  | TokenKind.Operator Operator.Divide -> "/"
  | TokenKind.Operator Operator.GreaterThan -> ">"
  | TokenKind.Operator Operator.GreaterThanOrEqual -> ">="
  | TokenKind.Operator Operator.LessThan -> "<"
  | TokenKind.Operator Operator.LessThanOrEqual -> "<="
  | TokenKind.Operator Operator.Minus -> "-"
  | TokenKind.Operator Operator.Multiply -> "*"
  | TokenKind.Operator Operator.Plus -> "+"
  | TokenKind.Boolean true -> "true"
  | TokenKind.Boolean false -> "false"
  | TokenKind.Number(s, _) -> s
  | TokenKind.String(s) -> "\"" + s.Replace("\\", "\\\\").Replace("\n", "\\n").Replace("\"", "\\\"") + "\""
  | TokenKind.Ident(i) -> i
  | TokenKind.QIdent(q) -> "'" + q + "'"
  | TokenKind.White(w) -> w
  | TokenKind.Newline -> "\n"
  | TokenKind.Error(c) -> string c
  | _ -> failwith "Unsupported token"

/// Turns series of tokens into string, using their Token value
let formatTokens (tokens:seq<Token>) = 
  tokens |> Seq.map (fun t -> formatToken t.Token) |> String.concat ""

/// Turn series of tokens into string, using their Range and original input
let formatTokensUsingRange (source:string) (tokens:seq<Token>) = 
  tokens 
  |> Seq.map (fun t -> source.Substring(t.Range.Start, t.Range.End - t.Range.Start))
  |> String.concat ""

/// Generate short, potentially empty random string using characters from the given input string
let randomString (s:string) = 
  Array.init (rnd.Next 10) (fun _ -> s.[rnd.Next(s.Length)]) |> System.String

/// List of tokens that do not have any parameters
let constantTokens = 
  [ TokenKind.LParen; TokenKind.RParen; TokenKind.Equals; TokenKind.Dot; TokenKind.Comma
    TokenKind.Let; TokenKind.LSquare; TokenKind.RSquare; TokenKind.Fun; TokenKind.Arrow
    TokenKind.Operator Operator.Divide; TokenKind.Operator Operator.GreaterThan
    TokenKind.Operator Operator.GreaterThanOrEqual; TokenKind.Operator Operator.LessThan
    TokenKind.Operator Operator.LessThanOrEqual; TokenKind.Operator Operator.Minus
    TokenKind.Operator Operator.Multiply; TokenKind.Operator Operator.Plus; TokenKind.Newline 
    TokenKind.Boolean true; TokenKind.Boolean false ]

/// Generate random token - takes last token to avoid generating token pairs that
/// would be parsed differently (e.g. Number 1; Number 2 would be Number 12)
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

/// Generate completely random string
let withString () =
  Array.init 100 (fun _ -> randomString "\"'\\\n abcDEF1234,[].+-/<>=@#$%") 
  |> String.concat ""

/// Generate random array of tokens (with incorrect ranges)
let withTokens () =   
  TokenKind.Dot
  |> Seq.unfold (fun last ->
    let t = { Token = randomToken last; Range = { Start = 0; End = 0 } }
    Some(t, t.Token))
  |> Seq.take (20 + rnd.Next(100))
  |> List.ofSeq

/// Check property involving random tokens
let check g (f:_ -> unit) = 
  for i in 1 .. 100 do
    f (g())
  printfn "      (Passed 100 tests)"

/// Check that tokens are equal (ignoring ranges)
let tokensEqual t1 t2 = 
  let equal = [ for t in t1 -> t.Token ] = [ for t in t2 -> t.Token ]
  if not equal then
    printfn "      *** Assertion failed: Tokens do not match ***"
    printfn "      *** Tokens:"
    let n = Seq.zip t1 t2 |> Seq.takeWhile (fun (t1, t2) -> t1 = t2) |> Seq.length
    Seq.zip t1 t2 
    |> Seq.skip (max 0 (n - 2))
    |> Seq.truncate 5
    |> Seq.iter (fun (t1, t2) ->
        printfn "      *** - '%s' %s '%s' " (formatToken t1.Token) (if t1 = t2 then "=" else "<>") (formatToken t2.Token))
  Assert.AreEqual(true, equal)

/// Shorter assertion
let equal (a:'T) (b:'T) = 
  if a <> b then 
    printfn "      *** Assertion failed: Values do not match ***"
    printfn "      *** Expected: %A" a
    printfn "      *** Actual: %A" b
  Assert.AreEqual(true, (a = b))

// --------------------------------------------------------------------------------------
// Helpers for writing property tests for tokenizer
// --------------------------------------------------------------------------------------

[<Test>]
let ``Format and tokenize returns original tokens`` () =
  check withTokens (fun sourceTokens ->  
    let parsedTokens, errors = Tokenizer.tokenize (formatTokens sourceTokens)
    tokensEqual parsedTokens sourceTokens)

[<Test>]
let ``Format and tokenize does not produce errors`` () =
  check withTokens (fun sourceTokens ->  
    let parsedTokens, errors = Tokenizer.tokenize (formatTokens sourceTokens)
    equal [||] errors)

[<Test>]
let ``Concatenate using ranges recreates original input for valid tokens`` () =
  check withTokens (fun sourceTokens ->  
    let input = formatTokens sourceTokens
    let parsedTokens, _ = Tokenizer.tokenize input
    let output = formatTokensUsingRange input parsedTokens
    equal input output)

[<Test>]
let ``Concatenate using ranges recreates original input for random string`` () =
  check withString (fun input ->  
    let parsedTokens, _ = Tokenizer.tokenize input
    let output = formatTokensUsingRange input parsedTokens
    equal input output)
