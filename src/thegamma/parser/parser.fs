// ------------------------------------------------------------------------------------------------
// Parser for TheGamma script langauge - turns Token[] into Program
// ------------------------------------------------------------------------------------------------
module TheGamma.Parser

open TheGamma
open TheGamma.Ast
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// This is mostly a recursive-descent parser
// ------------------------------------------------------------------------------------------------

/// Parsing context is mutated, because we never backtrack (except for one case,
/// in which case we clone the context explicitly using `clone`)
type ParsingContext = 
  { Tokens : Token[]
    Whitespace : ResizeArray<Token>
    Errors : ResizeArray<Error<Range>>
    mutable SilentMode : bool
    mutable Position : int }

module Context =
  /// Lets us implement lookahead withot making the whole context immutable
  /// (this is only used in fairly limited scenarios - e.g. named arguments)
  let clone ctx = 
    { Tokens = ctx.Tokens
      Whitespace = ResizeArray(ctx.Whitespace)
      Errors = ResizeArray(ctx.Errors)
      Position = ctx.Position
      SilentMode = ctx.SilentMode }

  /// Advance the position
  let next ctx = ctx.Position <- ctx.Position + 1

  /// Add specified error to context
  let error ctx e = if not ctx.SilentMode then ctx.Errors.Add(e)

  /// Run the given function with silent mode on
  let silent ctx f = 
    ctx.SilentMode <- true
    let res = f ctx
    ctx.SilentMode <- false    
    res

  /// Parses next token & accumulates whitespace. 
  /// Only retrns tokens if they are next or indented.
  let rec tokenIndent ctx = 
    match ctx.Tokens.[ctx.Position] with
    | { Token = TokenKind.Newline } as t1 ->
        match ctx.Tokens.[ctx.Position + 1] with
        | t2 & { Token = TokenKind.White s } ->
            next ctx; next ctx
            ctx.Whitespace.Add t1
            ctx.Whitespace.Add t2
            tokenIndent ctx
        | _ -> None
    | { Token = TokenKind.Error _ | TokenKind.White _ } as t ->
        ctx.Whitespace.Add t
        next ctx
        tokenIndent ctx
    | t -> 
        let white = ctx.Whitespace |> Seq.toList
        ctx.Whitespace.Clear()
        Some(white, t)

  /// Parses next token & accumulates whitespace. 
  /// Only retrns tokens if they are first or non-indented after a newline.
  let rec tokenNonIndent ctx = 
    match ctx.Tokens.[ctx.Position] with
    | { Token = TokenKind.Newline } as t1 ->

        // Find next Newline that is followed by non-whitespace (indented or not)
        let mutable newNonEmptyLinePos = ctx.Position
        let mutable i = ctx.Position
        while i < ctx.Tokens.Length do
          match ctx.Tokens.[i].Token with
          | TokenKind.Newline -> newNonEmptyLinePos <- i; i <- i + 1
          | TokenKind.White _ -> i <- i + 1
          | _ -> i <- ctx.Tokens.Length          
        ctx.Position <- newNonEmptyLinePos

        // If it was indented, then bad luck...
        match ctx.Tokens.[ctx.Position + 1] with
        | { Token = TokenKind.White _ } -> None
        | t -> 
            next ctx
            ctx.Whitespace.Add(t1)
            let white = ctx.Whitespace |> Seq.toList
            ctx.Whitespace.Clear()
            Some(white, t)

    | { Token = TokenKind.White _ } when ctx.Position = 0 -> None
    | t when ctx.Position = 0 || ctx.Position = ctx.Tokens.Length - 1 -> 
        Some([], t)
    | _ -> None

/// Creates a node with a range and no whitespace
let node rng n = 
  { Node = n; Range = rng; WhiteBefore = []; WhiteAfter = []; Entity = None }

/// Appends whitespace after node
let whiteAfter w n = { n with WhiteAfter = n.WhiteAfter @ w  }

/// Preppends whitespace beforenode
let whiteBefore w n = { n with WhiteBefore = w @ n.WhiteBefore }

/// Parsed token is identifier or quoted identifier (with preceding whitespace)
let (|Identifier|_|) t = 
  match t with
  | white, { Range = rng; Token = (TokenKind.Ident id | TokenKind.QIdent id) } ->
      node rng { Name.Name = id } |> whiteBefore white |> Some
  | _ -> None

// ------------------------------------------------------------------------------------------------
// Operator precedence handling
// ------------------------------------------------------------------------------------------------

type Associativity = Left | Right

let precedence = function
  | Operator.Equals -> 0, Left
  | Operator.GreaterThan | Operator.GreaterThanOrEqual
  | Operator.LessThan | Operator.LessThanOrEqual -> 1, Left
  | Operator.Plus | Operator.Minus -> 2, Left
  | Operator.Multiply | Operator.Divide -> 3, Left
  | Operator.Power -> 4, Right
  
/// Represnts a sequence of expressions separated by binary operators
/// (e.g. 'f x + 1 * 2 / g y' has 4 expressions separated by 3 operators)
type OpExpr = OpExpr of Node<Expr> * option<Node<Operator> * OpExpr>

/// Turn 'OpExpr' into a parsed 'Expr' using the "Precedence climbing method"
/// (see https://en.wikipedia.org/wiki/Operator-precedence_parser)
let rec precClimb minPrec (OpExpr(app, next)) =   
  let rec loop result (next:(Node<Operator>*OpExpr) option) = 
    match next with 
    | Some(op, next) when fst (precedence op.Node) >= minPrec ->
        let prec, assoc = precedence op.Node
        let nextMinPrec = 
          if assoc = Left then prec + 1 else prec
        let rhs, next = precClimb nextMinPrec next
        let result = node (unionRanges result.Range rhs.Range) (Expr.Binary(result, op, rhs))
        loop result next
    | _ -> result, next      
  loop app next

/// The terms are passed in reverse order as accumulated
let buildExpression terms term =
  terms 
  |> List.fold (fun oe (t, op) -> OpExpr(t, Some(op, oe))) (OpExpr(term, None))
  |> precClimb 0
  |> fst

// ------------------------------------------------------------------------------------------------
// The parser - member access and calls
// ------------------------------------------------------------------------------------------------

/// Try parsing input as '<id> = <expr>', if that does not work, treat it as <expr>
let rec parseExpressionOrNamedParam ctx = 
  let lookAheadCtx = Context.clone ctx
  match Context.tokenIndent lookAheadCtx with
  | Some(Identifier id) ->
      Context.next lookAheadCtx
      match Context.tokenIndent lookAheadCtx with
      | Some(white, ({ Token = TokenKind.Equals } as t)) ->
          // Replay what we did on lookahead context on the original context
          ignore (Context.tokenIndent ctx); Context.next ctx
          ignore (Context.tokenIndent ctx); Context.next ctx
          match parseExpression [] ctx with
          | Some expr -> Choice1Of2(whiteAfter white id, expr)
          | None -> 
              Errors.Parser.unexpectedTokenInArgList t.Range t.Token |> Context.error ctx
              Choice1Of2(whiteAfter white id, node { Start = id.Range.End; End = id.Range.End } Expr.Empty)
      | _ -> Choice2Of2(parseExpression [] ctx)
  | _ -> 
      Choice2Of2(parseExpression [] ctx)
    

/// Parse a comma separated list of expressions or named parameter assignments -- after `(`
and parseCallArgList afterComma lastRng acc ctx = 
  let parsed, acc = 
    match parseExpressionOrNamedParam ctx with
    | Choice2Of2(None) -> false, acc
    | Choice2Of2(Some e) -> true, { Name = None; Value = e }::acc
    | Choice1Of2(id, e) -> true, { Name = Some id; Value = e }::acc
  match Context.tokenIndent ctx with
  | Some(white, ({ Token = TokenKind.RParen } as t)) ->
      Context.next ctx
      if afterComma && not parsed then        
        Errors.Parser.unexpectedTokenInArgList lastRng TokenKind.RParen |> Context.error ctx
      t.Range, white, List.rev acc

  | Some(white, { Token = TokenKind.Comma; Range = lastRng }) when parsed ->
      Context.next ctx
      parseCallArgList true lastRng acc ctx

  | Some(_, t) when t.Token <> TokenKind.EndOfFile ->
      // Skip over unexpected, but correctly nested tokens
      Context.next ctx
      Errors.Parser.unexpectedTokenInArgList t.Range t.Token |> Context.error ctx
      Context.silent ctx (fun ctx -> 
        parseCallArgList afterComma t.Range acc ctx)
  | _ ->
      // Unexpected end of nesting - end argument list now
      Errors.Parser.unexpectedScopeEndInArgList lastRng |> Context.error ctx
      lastRng, [], List.rev acc


/// If something goes wrong inside placeholder, this skips over everything until `]`
and parsePlaceholderRecovery silent lastTokRng lastTokOpt whiteAcc ctx =
  match Context.tokenIndent ctx with
  | Some(white, { Token = TokenKind.RSquare; Range = lastTokRng }) ->
      Context.next ctx
      if not silent then Errors.Parser.unexpectedEndOfPlaceholder lastTokRng |> Context.error ctx
      lastTokRng, whiteAcc @ white

  | Some(white, t) when t.Token <> TokenKind.EndOfFile ->
      // Skip over unexpected, but correctly nested tokens
      Context.next ctx
      if not silent then Errors.Parser.unexpectedTokenInPlaceholder t.Range t.Token |> Context.error ctx
      parsePlaceholderRecovery true t.Range (Some t.Token) (whiteAcc @ white @ [t]) ctx

  | _ ->
      // Unexpected end of placeholder - end placeholder now
      if not silent then Errors.Parser.unexpectedScopeEndInPlaceholder lastTokRng lastTokOpt |> Context.error ctx
      lastTokRng, whiteAcc


/// Parse placeholder after parsing `[` -- the full syntax is `[ident: <expr>]`
and parsePlaceholder rngLSQuare ctx = 
  match Context.tokenIndent ctx with
  | Some(Identifier id & (_, tokId)) ->
      Context.next ctx
      match Context.tokenIndent ctx with
      | Some(whiteBeforeColon, { Token = TokenKind.Colon; Range = rngColon }) ->
          Context.next ctx
          match parseExpression [] ctx with
          | Some body ->
              match Context.tokenIndent ctx with
              | Some(whiteBeforeSquare, { Token = TokenKind.RSquare; Range = rngRSquare }) ->
                  Context.next ctx
                  rngRSquare, Expr.Placeholder(whiteAfter whiteBeforeColon id, whiteAfter whiteBeforeSquare body)
              | _ ->
                  // RECOVERY: Skip everything until `]` or end of indentation
                  let rng, white = parsePlaceholderRecovery false body.Range None [] ctx
                  rng, Expr.Placeholder(id, whiteAfter white body)
          | _ ->
              // RECOVERY: Skip everything until `]` or end of indentation
              let rng, white = parsePlaceholderRecovery false rngColon (Some TokenKind.Colon) [] ctx
              rng, Expr.Placeholder(id, whiteAfter white (node rng Expr.Empty))
      | _ ->
          // RECOVERY: Skip everything until `]` or end of indentation
          let rng, white = parsePlaceholderRecovery false id.Range (Some tokId.Token) [] ctx
          rng, Expr.Placeholder(id, whiteAfter white (node rng Expr.Empty))
    | _ ->
        // RECOVERY: Skip everything until `]` or end of indentation
        let rng, white = parsePlaceholderRecovery false rngLSQuare (Some TokenKind.LSquare) [] ctx
        rng, Expr.Placeholder(node rngLSQuare { Name = "" }, whiteAfter white (node rng Expr.Empty))

      
/// Parse `ident` after `.` in `.ident`; skips over non-idents after dot until it finds ident
and parseIdentAfterDot body prevDotRng prevDotTok ctx =
  match Context.tokenIndent ctx with
  | Some(Identifier id) ->
      Context.next ctx
      let body = Expr.Member(body, node id.Range (Expr.Variable id)) |> node (unionRanges body.Range id.Range)
      parseCallOrMember body ctx
  | Some(white, { Token = TokenKind.LSquare; Range = rngLSQuare }) ->
      Context.next ctx
      let rngRSquare, body = parsePlaceholder rngLSQuare ctx 
      let body = body |> node (unionRanges rngLSQuare rngRSquare) |> whiteBefore white 
      parseCallOrMember body ctx
  | Some(_, { Token = TokenKind.EndOfFile })
  | None ->
      // RECOVERY: Nothing after dot - return body so far
      Errors.Parser.unexpectedScopeEndAfterDot prevDotRng prevDotTok |> Context.error ctx 
      let emptyRng = { End = prevDotRng.End; Start = prevDotRng.End+1 }
      Expr.Member(body, node emptyRng (Expr.Variable(node emptyRng {Name=""})))
      |> node (unionRanges body.Range emptyRng)

  | Some(white, t) ->
      // RECOVERY: Wrong token after dot - skip and try next
      Context.next ctx
      Errors.Parser.unexpectedTokenAfterDot t.Range t.Token |> Context.error ctx 
      let emptyRng = { End = prevDotRng.End; Start = prevDotRng.End+1 }
      let body =
        Expr.Member(body, node emptyRng (Expr.Variable(node emptyRng {Name=""})))
        |> node (unionRanges body.Range emptyRng)
      Context.silent ctx (parseIdentAfterDot body prevDotRng prevDotTok)



/// Parse `.ident` or `(args)` after we parsed an expression specified as body
and parseCallOrMember body ctx = 
  match Context.tokenIndent ctx with
  | Some(white, { Token = TokenKind.LParen; Range = firstRng }) ->
      Context.next ctx
      let lastRng, white, args = parseCallArgList false firstRng [] ctx
      let body = 
        Expr.Call(body, whiteAfter white (node (unionRanges firstRng lastRng) args)) 
        |> node (unionRanges body.Range lastRng)
      // Parse more chain elements after `(args).`
      match Context.tokenIndent ctx with
      | Some(white, t & { Token = TokenKind.Dot }) ->
          Context.next ctx
          parseIdentAfterDot (whiteAfter white body) t.Range t.Token ctx
      | _ -> body
            
  | Some(white, t & { Token = TokenKind.Dot }) ->
      Context.next ctx
      parseIdentAfterDot (whiteAfter white body) t.Range t.Token ctx

  | _ -> body      

// ------------------------------------------------------------------------------------------------
// The parser - functions, lists
// ------------------------------------------------------------------------------------------------

/// We already parsed `fun`, parse the rest of the function, i.e. `<id> -> <expr>`
and parseFunction ctx funRng = 
  match Context.tokenIndent ctx with
  | Some(Identifier id) ->
      Context.next ctx
      match Context.tokenIndent ctx with
      | Some(whiteAfterId, { Token = TokenKind.Arrow; Range = rngArr }) ->
          Context.next ctx
          let body = 
            match parseExpression [] ctx with
            | Some body -> body
            | _ -> 
                Errors.Parser.missingBodyOfFunc (unionRanges funRng rngArr) |> Context.error ctx
                node { Start = rngArr.End; End = rngArr.End } Expr.Empty
          let rng = unionRanges funRng body.Range
          node rng (Expr.Function(whiteAfter whiteAfterId id, body)) |> Some

      | nt -> 
          // RECOVERY: Missing arrow - try parsing the body anyway
          let errRng, whiteAfterId = 
            match nt with
            | None -> unionRanges funRng id.Range, []
            | Some(whiteAfterId, t) -> t.Range, whiteAfterId
          Errors.Parser.missingArrowInFunc errRng |> Context.error ctx
          let body = 
            match parseExpression [] ctx with 
            | Some e -> e 
            | _ -> node {Start=id.Range.End; End=id.Range.End} Expr.Empty
          node (unionRanges funRng body.Range) 
            (Expr.Function(id, whiteBefore whiteAfterId body)) |> Some            
          
  // RECOVERY: Unexpected token or end of scope - return empty function
  | Some(white, t) ->
      Errors.Parser.unexpectedTokenAfterFun t.Range t.Token |> Context.error ctx
      let rng = { Start = funRng.End; End = funRng.End }
      node rng (Expr.Function(node rng {Name=""}, node rng Expr.Empty)) 
      |> whiteBefore white |> Some
  
  | None ->
      Errors.Parser.unexpectedScopeEndInFunc funRng |> Context.error ctx
      let rng = { Start = funRng.End; End = funRng.End }
      node rng (Expr.Function(node rng {Name=""}, node rng Expr.Empty)) |> Some
    

/// Parse expression followed by a list of more elements or closing square bracket
and parseListElements afterComma lastRng whiteStart startRng acc ctx =
  let parsed, acc =  
    match parseExpression [] ctx with
    | Some expr -> true, fun white -> (whiteAfter white expr)::acc
    | _ -> false, fun _ -> acc

  match Context.tokenIndent ctx with
  | Some(white, { Token = TokenKind.RSquare; Range = endRng }) ->
      Context.next ctx
      if not parsed && afterComma then
        Errors.Parser.unexpectedTokenInList lastRng TokenKind.Comma |> Context.error ctx
      node (unionRanges startRng endRng) (Expr.List(List.rev (acc []))) |> whiteBefore white |> Some

  | Some(white, { Token = TokenKind.Comma; Range = lastRng }) ->
      Context.next ctx
      if not parsed && afterComma then
        Errors.Parser.unexpectedTokenInList lastRng TokenKind.Comma |> Context.error ctx
      parseListElements true lastRng whiteStart startRng (acc white) ctx

  | Some(_, t) when t.Token <> TokenKind.EndOfFile ->
      // Skip over unexpected, but correctly nested tokens
      Context.next ctx
      Errors.Parser.unexpectedTokenInList t.Range t.Token |> Context.error ctx
      Context.silent ctx (fun ctx ->
        parseListElements afterComma t.Range whiteStart startRng (acc []) ctx)
  | _ ->
      // Unexpected end of nesting - end argument list now
      Errors.Parser.unexpectedScopeEndInList lastRng |> Context.error ctx
      node (unionRanges startRng lastRng) (Expr.List(List.rev (acc []))) |> Some


// ------------------------------------------------------------------------------------------------
// The parser - terms and expressions
// ------------------------------------------------------------------------------------------------

/// A term is a single thing inside expression involving operators, i.e.
///   <expression> := <term> <op> <term> <op> .. <op> <term>
and parseTerm ctx = 
  match Context.tokenIndent ctx with
  // Variable or call chain
  | Some(Identifier id) ->
      Context.next ctx
      parseCallOrMember (node id.Range (Expr.Variable id)) ctx |> Some

  // String, numeric and Boolean literals
  | Some(white, { Token = TokenKind.Number(_, n); Range = r }) ->
      Context.next ctx
      node r (Expr.Number n) |> whiteAfter white |> Some
  | Some(white, { Token = TokenKind.String(s); Range = r }) ->
      Context.next ctx
      node r (Expr.String s) |> whiteAfter white |> Some
  | Some(white, { Token = TokenKind.Boolean(b); Range = r }) ->
      Context.next ctx
      node r (Expr.Boolean b) |> whiteAfter white |> Some

  // Parse nested expressions starting with `(` or list starting with `[`
  | Some(white, ({ Token = TokenKind.LParen } as t)) ->
      Context.next ctx
      parseParenTermEnd (t::List.rev white) [] (parseExpression [] ctx) ctx
  | Some(white, ({ Token = TokenKind.LSquare } as t)) ->
      Context.next ctx
      parseListElements false t.Range white t.Range [] ctx

  | Some(white, ({ Token = TokenKind.Fun } as t)) ->
      Context.next ctx
      parseFunction ctx t.Range

  // Not a term, but that's fine
  | _ -> None 


/// Parse what follows after `(<expr>` - either `)` or some errors 
and parseParenTermEnd wb wa bodyOpt ctx =  
  // Create parenthesized expression body, or return empty expression if missing
  let makeBody wa =
    let body = 
      match bodyOpt with
      | Some body -> body
      | None -> 
          let rng = List.append [List.head wb] wa |> List.map (fun t -> t.Range) |> List.reduce unionRanges
          Errors.Parser.missingParenthesizedExpr rng |> Context.error ctx
          node rng Expr.Empty
    Some(body |> whiteBefore (List.rev wb) |> whiteAfter (List.rev wa))

  // Wait for ')', ignoring other nested tokens & ending on end of nesting
  match Context.tokenIndent ctx with
  | Some(white, ({ Token = TokenKind.RParen } as t)) -> 
      Context.next ctx
      makeBody (t::(List.append (List.rev white) wa))
  | Some(white, t) -> 
      Context.next ctx
      Errors.Parser.unexpectedTokenInParenthesizedExpr t.Range t.Token |> Context.error ctx
      Context.silent ctx (fun ctx ->
        parseParenTermEnd wb (t::(List.append (List.rev white) wa)) bodyOpt ctx)
  | None ->
      let rng = match bodyOpt with Some b -> b.Range | _ -> (List.head wb).Range
      Errors.Parser.unexpectedScopeEndInParenthesizedExpr rng |> Context.error ctx
      makeBody wa
            

/// Parse expression consisting of multiple terms & operators
and parseExpression terms ctx = 
  match terms, parseTerm ctx with
  | terms, Some term -> 
      match Context.tokenIndent ctx with
      // Followed by operator and more expressions
      | Some(white, ({ Token = TokenKind.Equals } as t)) ->
          Context.next ctx
          parseExpression ((term, whiteBefore white (node t.Range Operator.Equals))::terms) ctx
      | Some(white, ({ Token = TokenKind.Operator op } as t)) ->
          Context.next ctx
          parseExpression ((term, whiteBefore white (node t.Range op))::terms) ctx
      | Some(white, _) -> buildExpression terms term |> whiteAfter white |> Some  
      | None -> buildExpression terms term |> Some
          
  // Not an expression, return None
  | [], None -> None  

  // Nothing after operator - ignore operator, but parse preceding terms
  | (term, op)::terms, None -> 
      Errors.Parser.unexpectedEndAfterOperator op.Range (TokenKind.Operator op.Node) |> Context.error ctx
      Some(buildExpression terms term)


// ------------------------------------------------------------------------------------------------
// The parser - commands
// ------------------------------------------------------------------------------------------------

/// Parse expression, skipping all tokens that cannot be parsed
let rec parseLetBindingBody lastRng ctx = 
  match parseExpression [] ctx with
  | Some body -> body
  | None ->
      match Context.tokenIndent ctx with
      | None ->     
          Errors.Parser.unexpectedScopeEndInLet lastRng |> Context.error ctx
          node lastRng Expr.Empty
      | Some(white, t) ->
          Errors.Parser.unexpectedTokenInLetBinding t.Range t.Token |> Context.error ctx
          Context.silent ctx (fun ctx -> parseLetBindingBody t.Range ctx)


/// Skip all remaining nested tokens after a command
let rec skipNestedTokens firstTok white ctx = 
  match Context.tokenIndent ctx with 
  | None -> firstTok, white
  | Some(whiteBefore, t & { Token = TokenKind.EndOfFile }) ->
      (if firstTok = None then Some t else firstTok), white @ whiteBefore
  | Some(whiteBefore, t) ->
      Context.next ctx
      let firstTok = if firstTok = None then Some t else firstTok
      skipNestedTokens firstTok (white @ whiteBefore @ [t]) ctx
      

/// Parse the rest of the let binding after `let`, handling all sorts of errors
/// This returns parsed command together with all nested expressions after the command
/// (those should not be nested, but we accept them anyway & report error)
let parseLetBinding whiteBeforeLet rngLet ctx = 
  match Context.tokenIndent ctx with
  | Some(Identifier id) ->
      Context.next ctx
      match Context.tokenIndent ctx with
      | Some (whiteAfterId, { Token = TokenKind.Equals; Range = rngEq }) ->
          Context.next ctx
          let body = parseLetBindingBody rngEq ctx                
          Command.Let(whiteAfter whiteAfterId id, body)
          |> node (unionRanges rngLet body.Range) 
          |> whiteBefore whiteBeforeLet

      | Some(whiteAfterId, t) -> 
          // RECOVERY: Unexpected token after ident - try to parse the body anyway
          Errors.Parser.unexpectedTokenInLetBinding t.Range t.Token |> Context.error ctx
          let body = parseLetBindingBody t.Range ctx
          Command.Let(whiteAfter whiteAfterId id, body)
          |> node (unionRanges rngLet id.Range) 
          |> whiteBefore whiteBeforeLet

      | None ->
          // RECOVERY: End of block after ident - return binding with empty expression
          Errors.Parser.missingBodyInLetBinding id.Range |> Context.error ctx
          let body = node { Start = id.Range.End; End = id.Range.End } Expr.Empty
          Command.Let(id, body)
          |> node (unionRanges rngLet id.Range) 
          |> whiteBefore whiteBeforeLet
          
  | Some(whiteAfterLet, t) ->
      // RECOVERY: Unexpected token after let - try to parse body as expression & assume emtpy identifier
      Errors.Parser.unexpectedTokenInLetBinding t.Range t.Token |> Context.error ctx
      let letEndRng = { Start = rngLet.End; End = rngLet.End }
      let body = 
        match parseExpression [] ctx with 
        | Some e -> e
        | None -> 
            let firstSkipped, white = skipNestedTokens None [] ctx
            let skipRng = t::white |> List.map (fun t -> t.Range) |> List.reduce unionRanges
            node skipRng Expr.Empty |> whiteAfter (t::white)
      Command.Let(whiteBefore whiteAfterLet (node letEndRng { Name = "" } ), body)
      |> node (unionRanges rngLet body.Range) 
      |> whiteBefore whiteBeforeLet
      
  | None ->
      // RECOVERY: Missing body - return let binding with empty expression and empty identifier
      Errors.Parser.missingBodyInLetBinding rngLet |> Context.error ctx
      let rng = { Start = rngLet.End; End = rngLet.End }
      Command.Let(node rng { Name = "" }, node rng Expr.Empty)
      |> node rng |> whiteBefore whiteBeforeLet


/// A command is either top-level expression or let binding
let rec parseCommands acc ctx = 
  match Context.tokenNonIndent ctx with
  | Some(white, { Token = TokenKind.EndOfFile }) ->
      // Return commands & store the whitespace
      match acc with 
      | x::xs -> List.rev ({ x with WhiteAfter = white }::xs)
      | [] -> []
    
  | Some(whiteBeforeLet, tok & { Token = TokenKind.Let; Range = rngLet }) ->
      Context.next ctx
      let cmd = parseLetBinding whiteBeforeLet rngLet ctx 
      parseCommands (cmd::acc) ctx

  | Some(white, t) -> 
      // Treat command as top-level expression
      match parseExpression [] ctx with
      | Some expr -> 
          let cmd = node expr.Range (Command.Expr(expr)) |> whiteBefore white
          parseCommands (cmd::acc) ctx
      | None -> 
          // RECOVERY: Not an expression, just skip over the whole thing
          let _, white = skipNestedTokens None [] ctx
          Errors.Parser.unexpectedNestedTokenInCommand t.Range t.Token |> Context.error ctx
          let skipRng = t::white |> List.map (fun t -> t.Range) |> List.reduce unionRanges
          let cmd = node skipRng (Command.Expr(node skipRng Expr.Empty)) |> whiteAfter (t::white)
          parseCommands (cmd::acc) ctx

  | None ->
      // RECOVERY: Skip over all subsequent nested tokens
      let firstSkipped, white = skipNestedTokens None [] ctx
      let firstSkipped = firstSkipped.Value
      Errors.Parser.unexpectedNestedTokenInCommand firstSkipped.Range firstSkipped.Token |> Context.error ctx
      let skipRng = firstSkipped::white |> List.map (fun t -> t.Range) |> List.reduce unionRanges
      let cmd = node skipRng (Command.Expr(node skipRng Expr.Empty)) |> whiteAfter white
      parseCommands (cmd::acc) ctx


// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

let parseProgram (input:string) = 
  try
    let tokens, errors = Tokenizer.tokenize input
    let ctx = 
      { Tokens = tokens; Position = 0; SilentMode = false
        Errors = ResizeArray<_>(); Whitespace = ResizeArray<_>() }
    let cmds = parseCommands [] ctx
    let errors = Array.append errors (ctx.Errors.ToArray())
    let rng = cmds |> List.fold (fun rng cmd -> unionRanges rng cmd.Range) { Start = 0; End = 0 }
    { Body = node rng cmds }, errors
  with e ->
    Log.exn("parsing", "Exception while parsing program: %O", e)
    let rng = { Start=0; End=0 }
    let error = Errors.Parser.exceptionWhileParsing rng (e.ToString())
    { Body = node rng [] }, [| error |]
