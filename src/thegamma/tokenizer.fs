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
  char '\'' () <*> oneOrMore (pred (fun c -> c <> '\n' && c <> '\'')) <*> char '\'' ()
  |> map (fun ((_, cs), _) -> TokenKind.QIdent(System.String(Array.ofList cs)) )

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
  oneOrMore (pred (fun c -> c = ' ' || c = '\n'))
  |> map (fun whites -> TokenKind.White(System.String(Array.ofList whites)))

/// The main lexer for our mini-ML language
let rawTokens = 
  sequenceChoices 
    [ char '(' TokenKind.LParen
      char ')' TokenKind.RParen 
      char '=' TokenKind.Equals
      char '.' TokenKind.Dot
      char ',' TokenKind.Comma
      integer
      operator
      quotedIdent
      keywordOrIdent
      whitespace ] 

let incrementLocation loc tok = 
  match tok with
  | TokenKind.Dot
  | TokenKind.Comma
  | TokenKind.Equals
  | TokenKind.LParen
  | TokenKind.RParen -> { loc with Column = loc.Column + 1 }
  | TokenKind.Ident s 
  | TokenKind.Operator s 
  | TokenKind.Number(s, _) -> { loc with Column = loc.Column + s.Length }
  | TokenKind.Boolean(b) -> { loc with Column = loc.Column + if b then 4 else 5 }
  | TokenKind.QIdent s -> { loc with Column = loc.Column + s.Length + 2 }
  | TokenKind.Let -> { loc with Column = loc.Column + 3 }
  | TokenKind.White s ->
      s |> Seq.fold (fun loc c -> 
        if c = '\n' then { Line = loc.Line + 1; Column = 0 } 
        else { loc with Column = loc.Column +  1} ) loc

let tokens = rawTokens |> map (fun toks -> 
  let start = { Line = 1; Column = 0; }
  toks 
  |> List.scan (fun st tok ->
      { Token = tok; Range = { Start = st.Range.End; End = incrementLocation st.Range.End tok }})
    { Token = TokenKind.White ""; Range = { Start = start; End = start } }
  |> List.tail )
