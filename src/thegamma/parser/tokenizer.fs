// ------------------------------------------------------------------------------------------------
// Tokenizer for TheGamma script language - turns string into Token[]
// ------------------------------------------------------------------------------------------------
module TheGamma.Tokenizer
open TheGamma

/// Tokenization context for storing input, errors & parsed tokens
type Context = 
  { Tokens : ResizeArray<Token>
    Errors : ResizeArray<Error<Range>>
    Input : string }

/// Test whether 's' has 'prefix' at offset 'i'. The
/// parameter 'j' is index inside prefix where we're starting.
let rec startsWith (s:string) i j (prefix:string) = 
  if j = prefix.Length then true
  elif i = s.Length then false
  elif s.[i] <> prefix.[j] then false
  else startsWith s (i+1) (j+1) prefix

/// Is given character a string?
let letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

/// Is given character a number?
let number c = c >= '0' && c <= '9'


/// Add newly parsed token to the context, increment 
/// offset correctly & continue tokenizing
let rec addAndTokenize ctx tok i l =
  { Token = tok
    Range = { Start = i; End = i + l - 1 } } |> ctx.Tokens.Add 
  tokenizeInput ctx (i + l)


/// Tokenize identifier (continue consuming letters & characters)
and tokenizeIdent ctx start l =
  if start + l < ctx.Input.Length && 
      (letter ctx.Input.[start+l] || number ctx.Input.[start+l]) then
    tokenizeIdent ctx start (l+1)
  else
    addAndTokenize ctx (TokenKind.Ident(ctx.Input.Substring(start, l))) start l


/// Tokenize string (until end of input or closing double-quote)
and tokenizeString ctx acc start l =
  if start + l >= ctx.Input.Length then 
    tokenizeStringEnd true ctx acc start l
  else
    match ctx.Input.[start + l] with
    | '\\' when start + l + 1 >= ctx.Input.Length ->
        tokenizeStringEnd true ctx ('\\'::acc) start (l + 1)
    | '\\' ->
        match ctx.Input.[start + l + 1] with
        | 'n' -> tokenizeString ctx ('\n'::acc) start (l + 2)
        | 't' -> tokenizeString ctx ('\t'::acc) start (l + 2)
        | '\\' -> tokenizeString ctx ('\\'::acc) start (l + 2)
        | '"' -> tokenizeString ctx ('"'::acc) start (l + 2)
        | c -> tokenizeString ctx (c::'\\'::acc) start (l + 2)
    | '"' -> tokenizeStringEnd false ctx acc start (l + 1)
    | c -> tokenizeString ctx (c::acc) start (l+1)

and tokenizeStringEnd error ctx acc start l =
  let str = acc |> List.toArray |> Array.rev |> System.String
  let rng = { Start = start; End = start + l }
  if error then ctx.Errors.Add(Errors.Tokenizer.inputEndInsideString rng str) 
  addAndTokenize ctx (TokenKind.String(str)) start l


/// Tokenize quoted ident (until end of input or closing single-quote)
and tokenizeQuotedIdent ctx start l =
  if start + l >= ctx.Input.Length then 
    tokenizeQuotedIdentEnd true ctx start l
  else
    match ctx.Input.[start + l] with
    | '\n' -> tokenizeQuotedIdentEnd true ctx start (l + 1)
    | '\'' -> tokenizeQuotedIdentEnd false ctx start (l + 1)
    | c -> tokenizeQuotedIdent ctx start (l + 1)

and tokenizeQuotedIdentEnd error ctx start l =
  let rng = { Start = start; End = start + l }
  let qid = ctx.Input.Substring(start + 1, l - if error then 1 else 2)
  let qid = if qid.EndsWith("\n") then qid.Substring(0, qid.Length-1) else qid
  if error then ctx.Errors.Add(Errors.Tokenizer.missingClosingQuote rng qid) 
  addAndTokenize ctx (TokenKind.QIdent(qid)) start l


/// Tokenize whitespace - consume all spaces available
and tokenizeWhite ctx start l =
  if start + l < ctx.Input.Length && ctx.Input.[start+l] = ' ' then
    tokenizeWhite ctx start (l+1)
  else
    addAndTokenize ctx (TokenKind.White(ctx.Input.Substring(start, l))) start l


/// Tokenize number - consume all numbers, or '.' when 'decimal = false'
and tokenizeNumber ctx decimal start l =
  if start + l < ctx.Input.Length && number ctx.Input.[start+l] then
    tokenizeNumber ctx decimal start (l+1)
  elif start + l < ctx.Input.Length && not decimal && ctx.Input.[start+l] = '.' then
    tokenizeNumber ctx true start (l+1)
  else
    let str = ctx.Input.Substring(start, l)
    addAndTokenize ctx (TokenKind.Number(str, float str)) start l


and tokenizeInput ctx i = 
  // Reached the end of the input
  if i >= ctx.Input.Length then ctx else

  // Keyword or multi-letter symbol
  match ctx.Input.[i] with
  | '-' when startsWith ctx.Input i 0 "->" -> 
      addAndTokenize ctx (TokenKind.Arrow) i 2
  | 'f' when startsWith ctx.Input i 0 "fun" -> 
      addAndTokenize ctx (TokenKind.Fun) i 3
  | 'l' when startsWith ctx.Input i 0 "let" -> 
      addAndTokenize ctx (TokenKind.Let) i 3
  | 't' when startsWith ctx.Input i 0 "true" -> 
      addAndTokenize ctx (TokenKind.Boolean true) i 4
  | 'f' when startsWith ctx.Input i 0 "false" -> 
      addAndTokenize ctx (TokenKind.Boolean false) i 5
  | '<' when startsWith ctx.Input i 0 "<=" -> 
      addAndTokenize ctx (TokenKind.Operator Operator.LessThanOrEqual) i 2
  | '>' when startsWith ctx.Input i 0 ">=" -> 
      addAndTokenize ctx (TokenKind.Operator Operator.GreaterThanOrEqual) i 2

  // Single-letter tokens
  | '(' -> addAndTokenize ctx TokenKind.LParen i 1
  | ')' -> addAndTokenize ctx TokenKind.RParen i 1
  | '=' -> addAndTokenize ctx TokenKind.Equals i 1
  | '.' -> addAndTokenize ctx TokenKind.Dot i 1
  | ',' -> addAndTokenize ctx TokenKind.Comma i 1
  | ':' -> addAndTokenize ctx TokenKind.Colon i 1
  | '[' -> addAndTokenize ctx TokenKind.LSquare i 1
  | ']' -> addAndTokenize ctx TokenKind.RSquare i 1
  | '\n' -> addAndTokenize ctx TokenKind.Newline i 1

  // Single-letter operators
  | '>' -> addAndTokenize ctx (TokenKind.Operator(Operator.GreaterThan)) i 1
  | '<' -> addAndTokenize ctx (TokenKind.Operator(Operator.LessThan)) i 1
  | '+' -> addAndTokenize ctx (TokenKind.Operator(Operator.Plus)) i 1
  | '-' -> addAndTokenize ctx (TokenKind.Operator(Operator.Minus)) i 1
  | '*' -> addAndTokenize ctx (TokenKind.Operator(Operator.Multiply)) i 1
  | '/' -> addAndTokenize ctx (TokenKind.Operator(Operator.Divide)) i 1
  | '^' -> addAndTokenize ctx (TokenKind.Operator(Operator.Power)) i 1
  
  // Symbols that start something (string, whitespace, quoted ident)
  | '"' -> tokenizeString ctx [] i 1
  | ' ' -> tokenizeWhite ctx i 1
  | '\'' -> tokenizeQuotedIdent ctx i 1
  | c ->
  
  // Letter starts identifer, number starts number
  if letter c then tokenizeIdent ctx i 1
  elif number c then tokenizeNumber ctx false i 1
  else 

  // Otherwise report an error & skip one character
  ctx.Errors.Add(Errors.Tokenizer.unexpectedCharacter { Start = i; End = i } c)
  addAndTokenize ctx (TokenKind.Error c) i 1


/// Tokenize the given input. Consumes all input characters and returns
/// list of parsed tokens together with an array of tokenization errors.
let tokenize input = 
  let ctx = 
    { Errors = new ResizeArray<_>()
      Tokens = new ResizeArray<_>()
      Input = input }
  let ctx = tokenizeInput ctx 0
  ctx.Tokens.Add { Token = TokenKind.EndOfFile; Range = { Start = input.Length; End = input.Length } }
  ctx.Tokens.ToArray(), ctx.Errors.ToArray()
