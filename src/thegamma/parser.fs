// ------------------------------------------------------------------------------------------------
// turns list<Token> into Expr
// ------------------------------------------------------------------------------------------------
module TheGamma.Parser

open TheGamma
open TheGamma.Parsec

let anySpace = zeroOrMore (pred (fun t -> match t.Token with TokenKind.White _ -> true | _ -> false))
let anyWhite = zeroOrMore (pred (fun t -> match t.Token with TokenKind.Newline | TokenKind.White _ -> true | _ -> false))

let anyUntilLineEnd = Parser(fun input ->
  let rec loop input =
    match input with
    | offset, { Token = TokenKind.Newline }::rest -> Some((offset+1, rest), [], ())
    | offset, tok::rest -> loop (offset+1, rest)
    | _, [] -> None
  loop input)

// Parsing of simple expressions that correspond to tokens
let token tok = pred (fun t -> t.Token = tok)

let separated sep p =
  p <*> zeroOrMore (sep <*> p)
  |> map (fun (a1, args) -> a1::(List.map snd args))

let separatedThen sep p1 p2 =
  p1 <*> zeroOrMore (sep <*> p2)
  |> map (fun (a1, args) -> a1::(List.map snd args))

let separatedOrEmpty sep p = 
  optional (separated sep p) 
  |> map (fun l -> defaultArg l [])

let ident =
  anyWhite <*>> range (choose (fun tok ->
    match tok.Token with
    | TokenKind.Ident id
    | TokenKind.QIdent id -> Some(id)
    | _ -> None)) |> map (fun (rng, id) -> { Name = id; Range = rng })
  
let expressionSetter, expression = slot()

let argument = 
  ( (ident <<*> anyWhite <<*> token TokenKind.Equals <*> expression)
    |> map (fun (name, expr) -> { Name = Some name; Value = expr }) ) <|>
  ( expression
    |> map (fun expr -> { Name = None; Value = expr }) )

let argumentList =  
   anyWhite <*>>
   range 
    ( token TokenKind.LParen <*>> 
      separatedOrEmpty (anyWhite <*> token TokenKind.Comma) argument <<*> 
      // Recovery: If we don't find ), just consume everything until the end of line
      ( ignore ( anyWhite <*> token TokenKind.RParen ) <|> 
        ( range anyUntilLineEnd 
          |> bind (fun (rng, _) -> error (Errors.Parser.missingClosingParen rng)) ) ) )


type CallOrProperty = 
  { Name : Name
    Arguments : option<Range * list<Argument<unit>>> }
     
let callOrProperty =
  ( // Recovery: if the identifier is empty, but there is arg list, continue
    range (ident <*> optional argumentList |> map(fun (id, args) -> Some id, args)) <|> 
    range (optional ident <*> argumentList |> map(fun (id, args) -> id, Some args)) )
  |> bind (function
      | _, (Some(name), args) -> unit { Name = name; Arguments = args }
      | rng, (None, args) -> 
          error (Errors.Parser.emptyIdentifier rng) <*>>
          unit { Name = { Name = ""; Range = rng }; Arguments = args })

let callOrPropertyOrNothing = 
  callOrProperty <|>
  // Recovery: there is nothing after '.' in method chain
  ( range (unit ()) |> bind (fun (rng, _) ->
      error (Errors.Parser.nothingAfterDot rng) <*>>
      unit { Name = { Name = ""; Range = rng; }; Arguments = None }) )

let invocationChain =
  separatedThen (anyWhite <*> token TokenKind.Dot) callOrProperty callOrPropertyOrNothing
  |> bind (function 
    | first::chain ->
        let inst = { Expr = ExprKind.Variable first.Name; Range = first.Name.Range; Type = () }
        let parsed = chain |> List.fold (fun st item -> 
          let expr, r =
            match item.Arguments with
            | Some(arng, args) -> ExprKind.Call(st, item.Name, args), Ranges.unionRanges st.Range (Ranges.unionRanges item.Name.Range arng)
            | None -> ExprKind.Property(st, item.Name), Ranges.unionRanges st.Range item.Name.Range
          { Expr = expr; Range = r; Type = () }) inst
        ( match first.Arguments with
          | Some(rng, _) -> error (Errors.Parser.valueNotAfunction rng first.Name.Name)
          | _ -> unit () ) <*>> unit parsed
    | [] -> 
        failwith "Unexpected: Parsed empty chain")

let primitive = 
  anyWhite <*>> choose (function 
    | { Token = TokenKind.Number(_, n); Range = rng } -> 
        Some { Expr = ExprKind.Number(n); Range = rng; Type = () }
    | { Token = TokenKind.Boolean(b); Range = rng } -> 
        Some { Expr = ExprKind.Boolean(b); Range = rng; Type = () }
    | _ -> None)

let declaration = 
  anyWhite <*>> token TokenKind.Let <*>> ident <<*> anyWhite <<*> token TokenKind.Equals <*> expression

let letBinding =
  range declaration
  |> map (fun (rng, (name, expr)) -> 
      { Command = CommandKind.Let(name, expr); Range = rng })

expressionSetter.Set
  ( anyWhite <*>> 
    ( invocationChain <|> 
      primitive ) )

let program =
  sequenceChoices
    [ expression |> map (fun e -> { Command = CommandKind.Expr e; Range = e.Range })
      letBinding ]
  |> range
  |> map (fun (rng, cmds) -> { Body = cmds; Range = rng })

(*
let var = 
  choose (function { Token = TokenKind.Ident id; Range = r } -> Some { Expr = ExprKind.Variable id } | _ -> None)
let qvar = 
  choose (function Token.QIdent id -> Some(Typed.Typed((), Expr.QVar id)) | _ -> None)
let integer = 
  choose (function Token.Number n -> Some(Typed.Typed((), Expr.Number n)) | _ -> None)
let op = 
  choose (function Token.Operator s -> Some s | _ -> None)

// Parsing of patterns
let patIdent =
  ( choose (function Token.Ident id -> Some(Pattern.Var id) | _ -> None) <|>
    choose (function Token.QIdent id -> Some(Pattern.QVar id) | _ -> None) )
  |> map (fun p -> TypedPat((), p))

let rec patNested () = 
  ( token Token.LParen <*>
    pattern () <*>
    token Token.RParen )
  |> map (fun ((_, p), _) -> p)

let patOneOrTuple () = 
  ( patIdent <*>
    zeroOrMore (token Token.Comma <*> pattern ()) )
  |> map (fun (p, ps) -> 
    match ps with 
    | [] -> p
    | ps -> TypedPat((), Pattern.Tuple(p :: (List.map snd ps))) )
  
let pattern () = delay (fun () ->
  patNested () <|> patOneOrTuple ())
  

// Parsing of function applications let operators
type Associativity = Left | Right

let precedence = function
  | "+" | "-" -> 1, Left
  | "*" | "/" -> 2, Left
  | "^" -> 3, Right
  | _ -> failwith "Invalid operator name in <code>Parser.precedence</code>."

/// Represnts a sequence of expressions separated by binary operators
/// (e.g. 'f x + 1 * 2 / g y' has 4 expressions separated by 3 operators)
type OpExpr = OpExpr of Typed<unit> * option<string * OpExpr>

/// Turn 'OpExpr' into a parsed 'Expr' using the "Precedence climbing method"
/// (see https://en.wikipedia.org/wiki/Operator-precedence_parser)
let rec buildExpr minPrec (OpExpr(app, next)) = 
  let rec loop result next = 
    match next with 
    | Some(op, next) when fst (precedence op) >= minPrec ->
        let prec, assoc = precedence op
        let nextMinPrec = 
          if assoc = Left then prec + 1 else prec
        let rhs, next = buildExpr nextMinPrec next
        let result = Typed.Typed((), Expr.Binary(op, result, rhs))
        loop result next
    | _ -> result, next      
  loop app next

/// Parse '<term> <term> .. <term>' representing function application
let rec apps () = 
  oneOrMore (term ()) |> map (fun t -> 
      List.tail t |> List.fold (fun st v -> Typed.Typed((), Expr.App(st, v))) (List.head t))

/// Parse '<apps> <op> <apps> .. <apps>' representing expression with operators
let opExpr () = delay (fun () ->
  apps() <*> (optional (op <*> opExpr ()))
  |> map (fun (hd, tl) -> OpExpr(hd, tl)) )

/// Parse the same as 'opExpr' let then turn it into 'Expr' using 'buildExpr'
let expr () = 
  ( opExpr () <*> 
    zeroOrMore (token Token.Comma <*> opExpr ()) )
  |> map (fun (e, es) ->
      let exprs = e::(List.map snd es) |> List.map (buildExpr 1 >> fst)
      match exprs with 
      | [e] -> e
      | es -> Typed((), Expr.Tuple(es)) )

/// Parse an expression wrapped in brackets
let bracketed () = delay (fun () ->
  ( token Token.LParen <*>
    expr () <*>
    token Token.RParen )
  |> map (fun ((_, e), _) -> e) )

/// Parse let binding of the form 'let <pat> = <expr> in <expr>'
let binding () = delay (fun () ->
  ( token Token.Let <*>
    zeroOrMore(pattern ()) <*>
    token Token.Equals <*>
    expr () <*>
    token Token.In <*>
    expr () )
  |> map (fun (((((_, pats), _), assign), _), body) ->
    let pat, pats = List.head pats, List.rev (List.tail pats)
    let assign = pats |> List.fold (fun assign pat -> Typed.Typed((), Expr.Fun(pat, assign))) assign
    Typed.Typed((), Expr.Let(pat, assign, body))) )

/// Parse a function of the form 'fun <pat> .. <pat> -> <expr>'
let func () = delay (fun () ->
  ( token Token.Fun <*>
    oneOrMore (pattern ()) <*>
    token Token.Arrow <*>
    expr () )
  |> map (fun (((_, pats), _), body) -> 
    pats |> List.rev |> List.fold (fun body pat -> Typed.Typed((), Expr.Fun(pat, body))) body )) 

let prev () = delay (fun () ->
  ( token Token.Prev <*>
    term () )
  |> map (fun (_, body) ->
    Typed.Typed((), Expr.Prev(body)) ))

/// Parse a term (this handles most of the usual expressions)
let term () = delay (fun () ->
  func () <|>
  integer <|>
  var <|> 
  qvar <|>
  prev () <|>
  binding () <|>
  bracketed () )
 *)