// ------------------------------------------------------------------------------------------------
// Lexer for a mini-ML langauge - turns list<char> into list<Token>
// ------------------------------------------------------------------------------------------------

module TheGamma.Tokenizer

open TheGamma
open TheGamma.Parsec

// Lookup tables for supported keywords and operators
let keywords = Map.ofSeq [ "let", TokenKind.Let; "true", TokenKind.Boolean true; "false", TokenKind.Boolean false ]
let operators = set [ '+'; '-'; '*'; '/'; '^' ]

// Parsing basic things like letters and numbers
let char c token = prefix [c] token
let string (s:string) token = prefix (List.ofArray(s.ToCharArray())) token

let letter = pred (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
let number = pred (fun c -> (c >= '0' && c <= '9'))

let integer = oneOrMore number |> map (fun digits ->
  let str = System.String(Array.ofList digits)
  TokenKind.Number(str, 1.0 * float str) )

let operator = pred operators.Contains |> map (fun op ->
  TokenKind.Operator(op.ToString()) )

/// Identifier in single-quotes can contain anything except for end of line or '
let quotedIdent = 
  char '\'' () <*>> oneOrMore (pred (fun c -> c <> '\n' && c <> '\'')) <<*> char '\'' ()
  |> map (fun cs -> TokenKind.QIdent(System.String(Array.ofList cs)) )

/// Parse letter followed by zero or more letters and/or numbers
let keywordOrIdent = 
  letter <*> (zeroOrMore (letter <|> number))
  |> map (fun (c, cs) -> 
      let s = System.String(Array.ofList (c::cs))
      match keywords.TryFind s with
      | Some token -> token
      | _ -> TokenKind.Ident(s) )

/// We parse and keep whitespace, but filter it out later 
let whitespace =
  ( oneOrMore (pred ((=) ' '))
    |> map (fun whites -> TokenKind.White(System.String(Array.ofList whites))) ) <|>
  ( pred ((=) '\n') 
    |> map (fun _ -> TokenKind.Newline) )

let unclosedQuotedIdent = 
  range (char '\'' () <*>> oneOrMore (pred (fun c -> c <> '\n' && c <> '\'')))
  |> bind (fun (rng, ch) -> 
      let q = System.String(Array.ofList ch)
      error (Errors.Tokenizer.missingClosingQuote rng q) <*>>
      unit (TokenKind.QIdent(q)))

/// The main lexer for our mini-ML language
let tokens = 
  [ char '(' TokenKind.LParen
    char ')' TokenKind.RParen 
    char '=' TokenKind.Equals
    char '.' TokenKind.Dot
    char ',' TokenKind.Comma
    integer
    operator
    quotedIdent
    keywordOrIdent
    whitespace 
    // Error recovery
    unclosedQuotedIdent ] 
  |> List.map range
  |> sequenceChoices 
  |> map (List.map (fun (rng, tok) -> { Token = tok; Range = rng }))
