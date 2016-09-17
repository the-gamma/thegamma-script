// ------------------------------------------------------------------------------------------------
// Parser for TheGamma script langauge - turns Token[] into Program
// ------------------------------------------------------------------------------------------------
module TheGamma.Parser

open TheGamma
open TheGamma.Ast
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// This is mostly a recursive-descent parser with a lot of additional bookkeeping for decent
// error recovery - this is indentation sensitive and parameters after `(` always have to be
// indented further. The `nestedToken` function returns `None` if the currently nested block ends.
// In erroneous case, we generally skip all errors within block, but terminate the current
// syntactic structure at block end (and treat it as next thing) 
// ------------------------------------------------------------------------------------------------

/// Parsing context is mutated, because we never backtrack (except for one case,
/// in which case we clone the context explicitly using `clone`)
type Context = 
  { Tokens : Token[]
    Whitespace : ResizeArray<Token>
    Errors : ResizeArray<Error<Range>>
    mutable TopLevel : bool
    mutable Silent : bool
    mutable StrictlyNested : bool
    mutable IndentCurrent : int
    mutable IndentStack : (int * bool) list
    mutable Position : int }

/// Lets us implement lookahead withot making the whole context immutable
/// (this is only used in fairly limited scenarios - e.g. named arguments)
let clone ctx = 
  { Tokens = ctx.Tokens
    Whitespace = ResizeArray(ctx.Whitespace)
    Errors = ResizeArray(ctx.Errors)
    TopLevel = ctx.TopLevel
    Silent = ctx.Silent
    StrictlyNested = ctx.StrictlyNested
    IndentCurrent = ctx.IndentCurrent
    IndentStack = ctx.IndentStack 
    Position = ctx.Position }

/// Advance the position
let next ctx = 
  ctx.Position <- ctx.Position + 1

/// Temporarilly silence all error reports (we're in wrong state anyway)
let usingSilentMode ctx = 
  let prev = ctx.Silent
  ctx.Silent <- true
  { new System.IDisposable with
      member x.Dispose() = ctx.Silent <- prev }

/// Report error if we are not in silent mode
let addError ctx e = 
  if not ctx.Silent then ctx.Errors.Add(e)

/// Specify that all further tokens should be indented. If `current`, then
/// we use indentation of the current line, otherwise, we use indentation
/// of the next line (as set by `setLineIndent`).
let usingIndent current (tok:Token) ctx =
  let started = 
    match ctx.IndentStack with
    | (prev, true)::_ when prev > ctx.IndentCurrent -> 
        Errors.Parser.unindentedBlock tok.Range tok.Token |> addError ctx
        false
    | (prev, false)::_ when prev <> ctx.IndentCurrent -> 
        failwith "usingIndent: We forgot to set the top-stack line indentation"
    | _ ->
        ctx.IndentStack <- (ctx.IndentCurrent, current)::ctx.IndentStack
        true
  { new System.IDisposable with
      member x.Dispose() =
        match started, ctx.IndentStack with
        | true, t::stack -> ctx.IndentStack <- stack
        | false, _ -> ()
        | _ -> failwith "usingIndent: We lost item from an indentation stack" }

/// In this mode, we accept toknes that are not indented at line 0 
/// (which is useful when parsing erroneously nested top-level commands)
let usingTopLevelNesting ctx = 
  let prev = ctx.StrictlyNested
  ctx.StrictlyNested <- true
  match ctx.IndentStack with
  | x::xs -> ctx.IndentStack <- (0, true)::xs
  | _ -> ()
  { new System.IDisposable with
      member x.Dispose() = ctx.StrictlyNested <- prev }

/// When we are not at top-level, we can break indentation rules 
/// (and report an error)
let usingNonTopLevel ctx = 
  let prev = ctx.TopLevel
  ctx.TopLevel <- false
  { new System.IDisposable with
      member x.Dispose() = ctx.TopLevel <- prev }

/// Set current line indent after parsing a token
let setLineIndent ctx l = 
  ctx.IndentCurrent <- l
  match ctx.IndentStack with
  | (oldl, false)::stack when l <= oldl -> ctx.IndentStack <- (System.Int32.MaxValue, true)::stack
  | (oldl, false)::stack -> ctx.IndentStack <- (l, true)::stack
  | _ -> ()

/// Parses next non-white token & accumulates whitespace
let rec justToken ctx = 
  let current ctx = ctx.Tokens.[ctx.Position]
  match current ctx with
  | { Token = TokenKind.Newline } as t ->
      ctx.Whitespace.Add t
      next ctx
      match current ctx with
      | { Token = TokenKind.White s } as t ->          
          ctx.Whitespace.Add t
          setLineIndent ctx s.Length
          next ctx
      | _ -> 
          setLineIndent ctx 0
      justToken ctx
  | { Token = TokenKind.Error _ | TokenKind.White _ } as t ->
      ctx.Whitespace.Add t
      next ctx
      justToken ctx
  | t -> 
      t

/// Parses token and eats all whitespace before it
let token ctx = 
  let t = justToken ctx
  let white = ctx.Whitespace |> Seq.toList
  ctx.Whitespace.Clear()
  white, t

/// Parses nested token and eats all whitespace before it
let nestedToken ctx = 
  let white () =
    let white = ctx.Whitespace |> Seq.toList
    ctx.Whitespace.Clear()
    white
  let t = justToken ctx
  match ctx.IndentStack with
  | (indent, _)::_ when ctx.IndentCurrent > indent || (not ctx.StrictlyNested && ctx.IndentCurrent = indent) -> Some(white(), t)
  | [] -> Some(white(), t)
  | _ -> None

/// Creates a node with a range and no whitespace
let node rng n = 
  { Node = n; Range = rng; WhiteBefore = []; WhiteAfter = []; Entity = None }

/// Appends whitespace after node
let whiteAfter w n = { n with WhiteAfter = n.WhiteAfter @ w  }

/// Preppends whitespace beforenode
let whiteBefore w n = { n with WhiteBefore = w @ n.WhiteBefore }

/// Return range of the last element of call chain
let lastCallOrPropertyRange expr id =
  match expr with
  | { Node = Expr.Call(_, id, _) | Expr.Property(_, id) } -> id.Range 
  | _ -> expr.Range

/// Parsed token is identifier or quoted identifier (with preceding whitespace)
let (|Identifier|_|) t = 
  match t with
  | white, { Range = rng; Token = (TokenKind.Ident id | TokenKind.QIdent id) } ->
      node rng { Name.Name = id } |> whiteAfter white |> Some
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
// The parser
// ------------------------------------------------------------------------------------------------

let makeCallOrProp optInst prevId prevArgs =
  // Reconstruct previous bit of the chain from optInst/prevId/prevArgs
  match optInst, prevArgs with
  | Some inst, None -> node (unionRanges inst.Range prevId.Range) (Expr.Property(inst, prevId))
  | None, None -> node prevId.Range (Expr.Variable(prevId))
  | _, Some prevArgs -> 
      let fullRng = 
        match optInst with 
        | Some i -> unionRanges i.Range prevArgs.Range 
        | _ -> unionRanges prevId.Range prevArgs.Range
      node fullRng (Expr.Call(optInst, prevId, prevArgs))

/// Property access or method call after '.' in a nested block
let rec parseChain optInst prevId prevArgs ctx = 
  let inst = makeCallOrProp optInst prevId prevArgs
  match nestedToken ctx with
  | Some (Identifier id) ->
      next ctx
      parseMember (Some inst) id ctx

  | Some(_, t) ->      
      // Error: Expected identifier (after '.') but there was some other token
      // at the correct level of nesting, so skip it & try prsing next thing
      Errors.Parser.unexpectedTokenAfterDot t.Range t.Token |> addError ctx 
      if t.Token = TokenKind.EndOfFile then inst
      else 
        next ctx
        use _silent = usingSilentMode ctx
        parseChain optInst prevId prevArgs ctx
      
  | None ->
  match token ctx with
  | Identifier id ->
      // Error: There is an identifier after '.' but it is not properly indented
      let rng = lastCallOrPropertyRange inst
      Errors.Parser.unindentedIdentifierAfterDot id.Range rng id.Node.Name |> addError ctx 
      // If we are at top-level, we stop (to avoid consuming next line). 
      // If we are inside expression list and not all the way to the left, 
      //  we continue - but for that, we unindent the current stack
      match ctx.TopLevel, ctx.IndentStack with 
      | false, (sl, si)::stack when ctx.IndentCurrent > 0 ->
          next ctx
          ctx.IndentStack <- (ctx.IndentCurrent, si)::stack
          parseMember (Some inst) id ctx
      | _ -> inst

  | _, t ->      
      // Error: Expected more after '.' but the nested scope ends here
      // Just return what we got so far & end the current chain
      let rng = lastCallOrPropertyRange inst
      Errors.Parser.unexpectedScopeEndAfterDot t.Range rng t.Token |> addError ctx 
      inst


/// Helper used by 'parseMember' - parse '.' or '(...)' after ident in a chain
and parseDotOrLParen optInst id ctx whiteAndTok = 
  match whiteAndTok with
  | white, ({ Token = TokenKind.LParen } as startTok)->
      next ctx
      use _top = usingNonTopLevel ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      let endRange, white, args = parseCallArgList false startTok.Range [] ctx
      let args = node (unionRanges startTok.Range endRange) args |> whiteAfter white
      
      // Call can be followed by '.' or end of call chain
      match nestedToken ctx with
      | Some(white, { Token = TokenKind.Dot }) ->
          next ctx
          Some(parseChain optInst id (Some args) ctx)
      | _ ->
          Some(makeCallOrProp optInst id (Some args))

  | white, { Token = TokenKind.Dot } ->
      next ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      Some(parseChain optInst id None ctx)

  | _ -> None


/// Call chain after name - either '.' & more or '(...)' or end of call chain
and parseMember (optInst:option<_>) (id:Node<Name>) ctx : Node<_> = 
  let parsed = 
    // Token is correctly nested - parse '.' or '('
    match nestedToken ctx with
    | Some res -> parseDotOrLParen optInst id ctx res
    | _ ->
    // Token is not nested, but it is '.' or '(', so we accept it with erorr
    let after = token ctx    
    match (use _silent = usingSilentMode ctx in parseDotOrLParen optInst id ctx after) with
    | Some res -> 
        Errors.Parser.unindentedDotAfterIdentifier id.Range (snd after).Range |> addError ctx 
        Some res 
    // Otherwise, we end the call chain
    | _ -> None
  match parsed with
  | Some res -> res
  | None -> 
      // If we did not parse anything, create chain with what we have
      match optInst with
      | Some inst -> node (unionRanges inst.Range id.Range) (Expr.Property(inst, id))
      | None -> node id.Range (Expr.Variable(id))


/// A term is a single thing inside expression involving operators, i.e.
///   <expression> := <term> <op> <term> <op> .. <op> <term>
and parseTerm ctx = 
  let tt = nestedToken ctx
  match tt with
  // Variable or call chain
  | Some((Identifier id) & (_, tok)) ->
      next ctx
      use _indent = usingIndent false tok ctx
      let varOrCall = parseMember None id ctx 
      Some varOrCall

  // String, numeric and Boolean literals
  | Some(white, { Token = TokenKind.Number(_, n); Range = r }) ->
      next ctx
      node r (Expr.Number n) |> whiteAfter white |> Some
  | Some(white, { Token = TokenKind.String(s); Range = r }) ->
      next ctx
      node r (Expr.String s) |> whiteAfter white |> Some
  | Some(white, { Token = TokenKind.Boolean(b); Range = r }) ->
      next ctx
      node r (Expr.Boolean b) |> whiteAfter white |> Some

  // Parse nested expressions starting with `(` or list starting with `[`
  | Some(white, ({ Token = TokenKind.LParen } as t)) ->
      next ctx
      parseParenTermEnd (t::List.rev white) [] (parseExpression [] ctx) ctx
  | Some(white, ({ Token = TokenKind.LSquare } as t)) ->
      next ctx
      use _nest = usingNonTopLevel ctx
      parseListElements false t.Range white t.Range [] ctx

  // Not a term, but that's fine
  | _ -> None


/// Parse list of elements and closing square bracket, after `[`
and parseListElements expectMore lastRng whiteStart startRng acc ctx =
  let parsed, acc =  
    match parseExpression [] ctx with
    | Some expr -> true, fun white -> (whiteAfter white expr)::acc
    | _ -> false, fun _ -> acc

  match nestedToken ctx with
  | Some(white, { Token = TokenKind.RSquare; Range = endRng }) ->
      next ctx
      node (unionRanges startRng endRng) (Expr.List(List.rev (acc []))) |> whiteBefore white |> Some
  | Some(white, { Token = TokenKind.Comma; Range = lastRng }) ->
      next ctx
      if not parsed && expectMore then
        Errors.Parser.unexpectedTokenInList lastRng TokenKind.Comma |> addError ctx
      parseListElements true lastRng whiteStart startRng (acc white) ctx
  | Some(_, t) ->
      // Skip over unexpected, but correctly nested tokens
      next ctx
      Errors.Parser.unexpectedTokenInList t.Range t.Token |> addError ctx
      parseListElements expectMore t.Range whiteStart startRng (acc []) ctx
  | None ->
      // Unexpected end of nesting - end argument list now
      Errors.Parser.unexpectedScopeEndInList lastRng |> addError ctx
      node (unionRanges startRng lastRng) (Expr.List(List.rev (acc []))) |> Some


/// Parse what follows after `(<expr>` - either `)` or some errors 
and parseParenTermEnd wb wa bodyOpt ctx =  
  // Create parenthesized expression body, or return empty expression if missing
  let makeBody wa =
    let body = 
      match bodyOpt with
      | Some body -> body
      | None -> 
          let rng = List.append [List.head wb] wa |> List.map (fun t -> t.Range) |> List.reduce unionRanges
          Errors.Parser.missingParenthesizedExpr rng |> addError ctx
          node rng Expr.Empty
    Some(body |> whiteBefore (List.rev wb) |> whiteAfter (List.rev wa))

  // Wait for ')', ignoring other nested tokens & ending on end of nesting
  match nestedToken ctx with
  | Some(white, ({ Token = TokenKind.RParen } as t)) -> 
      next ctx
      makeBody (t::(List.append (List.rev white) wa))
  | Some(white, t) -> 
      next ctx
      Errors.Parser.unexpectedTokenInParenthesizedExpr t.Range t.Token |> addError ctx
      parseParenTermEnd wb (t::(List.append (List.rev white) wa)) bodyOpt ctx
  | None ->
      let rng = match bodyOpt with Some b -> b.Range | _ -> (List.head wb).Range
      Errors.Parser.unindentedTokenInParenthesizedExpr rng |> addError ctx
      makeBody wa
            

/// Parse expression consisting of multiple terms & operators
and parseExpression terms ctx = 
  match terms, parseTerm ctx with
  | terms, Some term -> 
      match nestedToken ctx with
      // Followed by operator and more expressions
      | Some(white, ({ Token = TokenKind.Equals } as t)) ->
          next ctx
          parseExpression ((term, whiteBefore white (node t.Range Operator.Equals))::terms) ctx
      | Some(white, ({ Token = TokenKind.Operator op } as t)) ->
          next ctx
          parseExpression ((term, whiteBefore white (node t.Range op))::terms) ctx
      | _ -> 
          Some(buildExpression terms term)  
  // Not an expression, return None
  | [], None -> None  
  // Nothing after operator - ignore operator, but parse preceding terms
  | (term, op)::terms, None -> 
      let next = justToken ctx
      Errors.Parser.unexpectedTokenAfterOperator next.Range (TokenKind.Operator op.Node) next.Token |> addError ctx
      Some(buildExpression terms term)


/// Try parsing input as '<id> = <expr>', if that does not work, treat it as <expr>
and parseExpressionOrNamedParam ctx = 
  let lookAheadCtx = clone ctx
  match nestedToken lookAheadCtx with
  | Some(Identifier id) ->
      next lookAheadCtx
      match nestedToken lookAheadCtx with
      | Some(white, ({ Token = TokenKind.Equals } as t)) ->
          // Replay what we did on lookahead context on the original context
          ignore (nestedToken ctx); next ctx
          ignore (nestedToken ctx); next ctx
          match parseExpression [] ctx with
          | Some expr -> Choice1Of2(whiteAfter white id, expr)
          | None -> 
              Errors.Parser.unexpectedTokenInArgList t.Range t.Token |> addError ctx
              Choice2Of2(Some(node id.Range (Expr.Variable(id))))
      | _ -> Choice2Of2(parseExpression [] ctx)
  | _ -> 
      Choice2Of2(parseExpression [] ctx)
    

/// Parse a comma separated list of expressions or named parameter assignments
and parseCallArgList expectMore lastRng acc ctx = 
  let parsed, acc = 
    match parseExpressionOrNamedParam ctx with
    | Choice2Of2(None) -> false, acc
    | Choice2Of2(Some e) -> true, { Name = None; Value = e }::acc
    | Choice1Of2(id, e) -> true, { Name = Some id; Value = e }::acc
  match nestedToken ctx with
  | Some(white, ({ Token = TokenKind.RParen } as t)) ->
      next ctx
      if expectMore && not parsed then        
        Errors.Parser.unexpectedTokenInArgList lastRng TokenKind.RParen |> addError ctx
      t.Range, white, List.rev acc

  | Some(white, { Token = TokenKind.Comma; Range = lastRng }) when parsed ->
      next ctx
      parseCallArgList true lastRng acc ctx

  | Some(_, t) ->
      // Skip over unexpected, but correctly nested tokens
      next ctx
      Errors.Parser.unexpectedTokenInArgList t.Range t.Token |> addError ctx
      parseCallArgList expectMore t.Range acc ctx
  | None ->
      // Unexpected end of nesting - end argument list now
      Errors.Parser.unexpectedScopeEndInArgList lastRng |> addError ctx
      lastRng, [], List.rev acc


/// Parse a top-level expression, 
let rec parseNestedExpressions wacc acc ctx = 
  match parseExpression [] ctx with
  | Some expr ->  
      if not (List.isEmpty acc) then 
        Errors.Parser.nestedExpressionInCommand expr.Range |> addError ctx
        parseNestedExpressions [] ((whiteBefore (List.rev wacc) expr)::acc) ctx
      else
        use _strict = usingTopLevelNesting ctx
        parseNestedExpressions [] ((whiteBefore (List.rev wacc) expr)::acc) ctx
  | _ ->
  match nestedToken ctx with
  | Some(_, { Token = TokenKind.EndOfFile })
  | None ->
      match acc with
      | x::xs -> (whiteAfter (List.rev wacc) x)::xs
      | [] -> []
  | Some(white, tok) ->
      next ctx
      parseNestedExpressions (tok::(List.rev white) @ wacc) acc ctx


/// Parse the rest of the let binding after `let`, handling all sorts of errors
/// This returns parsed command together with all nested expressions after the command
/// (those should not be nested, but we accept them anyway & report error)
let parseLetBinding whiteBeforeLet rngLet ctx = 
  match nestedToken ctx with
  | Some(Identifier id) ->
      next ctx
      match nestedToken ctx with
      | Some (whiteAfterId, { Token = TokenKind.Equals; Range = rngEq }) ->
          next ctx
          match List.rev (parseNestedExpressions [] [] ctx) with
          | body::rest ->
              rest, 
              Command.Let(whiteAfter whiteAfterId id, body)
              |> node (unionRanges rngLet body.Range) 
              |> whiteBefore whiteBeforeLet
              
          | [] ->
              // Missing body - return let binding with empty expression
              Errors.Parser.missingBodyInLetBinding (unionRanges rngLet rngEq) |> addError ctx
              [],
              Command.Let(whiteAfter whiteAfterId id, node { Start = rngEq.End; End = rngEq.End } Expr.Empty)
              |> node (unionRanges rngLet rngEq) 
              |> whiteBefore whiteBeforeLet
              
      | Some (whiteAfterId, t) ->
          // Unexpected token after ident - try to parse nested body as expression nevertheless
          Errors.Parser.unexpectedTokenInLetBinding t.Range t.Token |> addError ctx
          let body, rest = 
            match List.rev (parseNestedExpressions [] [] ctx) with
            | body::rest -> body, rest
            | [] -> node { Start = id.Range.End; End = id.Range.End } Expr.Empty, []
          rest,
          Command.Let(whiteAfter whiteAfterId id, body)
          |> node (unionRanges rngLet id.Range) 
          |> whiteBefore whiteBeforeLet
          
      | None ->
          // End of block after ident - return binding with empty expression
          Errors.Parser.missingBodyInLetBinding id.Range |> addError ctx
          let body = node { Start = id.Range.End; End = id.Range.End } Expr.Empty
          [], node (unionRanges rngLet id.Range) (Command.Let(id, body)) |> whiteBefore whiteBeforeLet
          
  | Some(whiteAfterLet, t) ->
      // Unexpected token after let - try to parse nested body as expression & assume emtpy identifier
      //printfn "Unexpected token after let: %A" t
      Errors.Parser.unexpectedTokenInLetBinding t.Range t.Token |> addError ctx
      let letEndRng = { Start = rngLet.End; End = rngLet.End }
      let body, rest = 
        match List.rev (parseNestedExpressions [] [] ctx) with
        | body::rest -> body, rest
        | [] -> node letEndRng Expr.Empty, []
      rest,
      Command.Let(whiteBefore whiteAfterLet (node letEndRng { Name = "" } ), body)
      |> node (unionRanges rngLet body.Range) 
      |> whiteBefore whiteBeforeLet
      
  | None ->
      // Missing body - return let binding with empty expression and empty identifier
      Errors.Parser.missingBodyInLetBinding rngLet |> addError ctx
      let rng = { Start = rngLet.End; End = rngLet.End }
      [], node rng (Command.Let(node rng { Name = "" }, node rng Expr.Empty)) |> whiteBefore whiteBeforeLet

      
/// A command is either top-level expression or let binding
let rec parseCommands acc ctx = 
  let c = token ctx
  match c with
  | whiteBeforeLet, ({ Token = TokenKind.Let; Range = rngLet } as tok) ->
      next ctx
      let rest, parsed = 
        use _indent = usingIndent false tok ctx
        parseLetBinding whiteBeforeLet rngLet ctx
      let rest = rest |> List.map (fun e -> node e.Range (Command.Expr e))
      parseCommands (rest @ (parsed::acc)) ctx

  | white, { Token = TokenKind.EndOfFile } ->
      // Return commands & store the whitespace
      match acc with 
      | x::xs -> List.rev ({ x with WhiteAfter = white }::xs)
      | [] -> []
    
  | white, tok -> 
      // Treat command as top-level expression
      let cmds = 
        use _indent = usingIndent true tok ctx
        parseNestedExpressions (List.rev white) [] ctx |> List.map (fun expr ->
          node expr.Range (Command.Expr expr))
      parseCommands (cmds @ acc) ctx

// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

let parseProgram (input:string) = 
  try
    let tokens, errors = Tokenizer.tokenize input
    let ctx = 
      { Tokens = tokens
        TopLevel = true; Silent = false; StrictlyNested = false
        Position = 0; IndentCurrent = 0; IndentStack = []
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
