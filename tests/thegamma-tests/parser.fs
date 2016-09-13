#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Parser
#endif
open TheGamma
open TheGamma.AST2
open TheGamma.AstOperations.AST2
open NUnit.Framework

// --------------------------------------------------------------------------------------
// We keep track of nesting. nestedToken returns None if the nested block ended.
// We generally skip all errors within a block, but terminte what we're parsing currently at block end
// --------------------------------------------------------------------------------------


let unionRanges r1 r2 =
  { Start = min r1.Start r2.Start
    End = max r1.End r2.End }


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

let next ctx = 
  ctx.Position <- ctx.Position + 1

let addWhitespace ctx w = 
  ctx.Whitespace.Add(w)

let getWhitespace ctx  =
  let white = ctx.Whitespace |> Seq.toList
  ctx.Whitespace.Clear()
  white

let setLineIndent ctx l = 
  ctx.IndentCurrent <- l
  match ctx.IndentStack with
  | (oldl, false)::stack when l <= oldl -> ctx.IndentStack <- (System.Int32.MaxValue, true)::stack
  | (oldl, false)::stack -> ctx.IndentStack <- (l, true)::stack
  | _ -> ()

let addError ctx e = 
  if not ctx.Silent then ctx.Errors.Add(e)

let usingIndent current (tok:Token) ctx =
  let started = 
    match ctx.IndentStack with
    | (prev, true)::_ when prev > ctx.IndentCurrent -> 
        Errors.Parser.unindentedBlock tok.Range tok.Token |> addError ctx
        false
    | (prev, false)::_ when prev <> ctx.IndentCurrent -> 
        failwith "TODO: startIndent - forgot to initialize line"
    | _ ->
        ctx.IndentStack <- (ctx.IndentCurrent, current)::ctx.IndentStack
        true

  { new System.IDisposable with
      member x.Dispose() =
        match started, ctx.IndentStack with
        | true, t::stack -> ctx.IndentStack <- stack
        | false, _ -> ()
        | _ -> failwith "error - endIndent when stack is empty" }

let usingStrictNesting ctx = 
  let prev = ctx.StrictlyNested
  ctx.StrictlyNested <- true
  match ctx.IndentStack with
  | x::xs -> ctx.IndentStack <- (0, true)::xs
  | _ -> ()
  { new System.IDisposable with
      member x.Dispose() = ctx.StrictlyNested <- prev }

let usingSilentMode ctx = 
  let prev = ctx.Silent
  ctx.Silent <- true
  { new System.IDisposable with
      member x.Dispose() = ctx.Silent <- prev }

let usingNestedMode ctx = 
  let prev = ctx.TopLevel
  ctx.TopLevel <- false
  { new System.IDisposable with
      member x.Dispose() = ctx.TopLevel <- prev }

let current ctx = ctx.Tokens.[ctx.Position]

let rec justToken ctx = 
  match current ctx with
  | { Token = TokenKind.Newline } as t ->
      addWhitespace ctx t
      next ctx
      match current ctx with
      | { Token = TokenKind.White s } as t ->          
          addWhitespace ctx t
          setLineIndent ctx s.Length
          next ctx
      | _ -> 
          setLineIndent ctx 0
      justToken ctx
  | { Token = TokenKind.White _ } as t ->
      addWhitespace ctx t
      next ctx
      justToken ctx
  | t -> 
      t

let token ctx = 
  let t = justToken ctx
  getWhitespace ctx, t

let nestedToken ctx = 
  let t = token ctx
  let res =
    match ctx.IndentStack with
    | (indent, _)::_ when ctx.IndentCurrent > indent || (not ctx.StrictlyNested && ctx.IndentCurrent = indent) -> Some t
    | [] -> Some t // failwith "TODO: Should not check nestedToken with no indent stack"
    | _ -> None
  if ctx.StrictlyNested then
    () //printfn "%d (%A) -- %A\n   returning: %A" ctx.IndentCurrent ctx.IndentStack t  res
  res

let node rng n = 
  { Node = n; Range = rng; WhiteBefore = []; WhiteAfter = [] }

let whiteAfter w n = { n with WhiteAfter = n.WhiteAfter @ w  }
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
      Some { WhiteBefore = white; WhiteAfter = []; Range = rng; Node = { Name.Name = id } }
  | _ -> None

// --------------------------------------------------------------------------------------
// Operator precedence handling
// --------------------------------------------------------------------------------------

// Parsing of function applications let operators
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

// --------------------------------------------------------------------------------------
// The parser
// --------------------------------------------------------------------------------------

let makeCallOrProp optInst prevId prevArgs =
  // Reconstruct previous bit of the chain from optInst/prevId/prevArgs
  match optInst, prevArgs with
  | Some inst, { Node = [] } -> node (unionRanges inst.Range prevId.Range) (Expr.Property(inst, prevId))
  | None, { Node = [] } -> node prevId.Range (Expr.Variable(prevId))
  | _ -> 
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
      use _top = usingNestedMode ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      let endRange, white, args = parseCallArgList false startTok.Range [] ctx
      let args = node (unionRanges startTok.Range endRange) args |> whiteAfter white
      
      // Call can be followed by '.' or end of call chain
      match nestedToken ctx with
      | Some(white, { Token = TokenKind.Dot }) ->
          next ctx
          Some(parseChain optInst id args ctx)
      | _ ->
          Some(makeCallOrProp optInst id args)

  | white, { Token = TokenKind.Dot } ->
      next ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      Some(parseChain optInst id (node { Start=0; End=0; } []) ctx)

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
      use _nest = usingNestedMode ctx
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
        use _strict = usingStrictNesting ctx
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

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

/// Tokenize & create default context
let tokenize input = 
  { Tokens = Tokenizer.tokenize input |> fst
    TopLevel = true
    Silent = false
    StrictlyNested = false
    Position = 0
    IndentCurrent = 0
    IndentStack = []
    Errors = ResizeArray<_>()
    Whitespace = ResizeArray<_>() }

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Assert that result contains given errors
let assertErrors expectErrors ((code:string), cmds, errs) = 
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, (astart, aend)) in List.zip expectErrors errs do
    let s = code.Substring(astart, aend-astart+1)
    equal en an
    equal ec s

/// Assert that expression contains given sub-expression
let rec hasSubExpr f e =
  if f e then true else
    match e with 
    | ExprNode(es, _) -> es |> List.exists (fun e -> hasSubExpr f e.Node)
    | ExprLeaf _ -> false

/// Assert that result contains given sub-expression
let assertSubExpr f (code, (cmds:Node<Command> list), errs) = 
  let matches = 
    cmds |> List.exists (fun cmd ->
      match cmd.Node with
      | Command.Expr e -> hasSubExpr f e.Node
      | Command.Let(_, e) -> hasSubExpr f e.Node )
  equal true matches

/// Assert that result contains binding command with body
let assertLet n f (code, (cmds:Node<Command> list), errs) = 
  let matches = cmds |> List.exists (fun cmd ->
    match cmd.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | _ -> false)
  equal true matches

/// Assert that result consists of the specified commands
let assertCmds ns fs (code, (cmds:Node<Command> list), errs) = 
  equal (List.length ns) (List.length cmds) 
  let matches = List.zip3 ns fs cmds |> List.forall (fun (n, f, c) -> 
    match c.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | Command.Expr(e) when n = "" -> f e.Node
    | _ -> false )
  equal true matches

/// Sub-expression contains property with given name
let isProperty name = function 
  | Expr.Property(_, n) -> n.Node.Name = name | _ -> false

/// Sub-expression contains call with given name and arguments match function
let isCall name ac = function 
  | Expr.Call(_, n, args) -> n.Node.Name = name && ac args | _ -> false

/// Sub-expression is a list with elements matching specified functions
let isList conds = function 
  | Expr.List(elems) -> List.zip conds elems |> List.forall (fun (cond, el) -> cond el.Node) | _ -> false

/// Expression is binary operator
let isBinary op fl fr = function
  | Expr.Binary(l, o, r) -> o.Node = op && fl l.Node && fr r.Node
  | _ -> false

/// Expression is specified value (string, int, float, bool)
let isVal (v:obj) e = 
  match e with
  | Expr.Number(n) -> (unbox v) = n
  | Expr.String(s) -> (unbox v) = s
  | Expr.Boolean(b) -> (unbox v) = b
  | _ -> false

/// Matches anything
let any _ = true

/// Specify conditions on arguments
let hasArgValues conds { Node = args } = 
  List.zip conds args |> List.forall (fun (f, (arg:Argument)) -> f arg.Value.Node)

/// Specify conditions on argument names
let hasArgNames names { Node = args } = 
  List.zip names args |> List.forall (fun (n, (arg:Argument)) -> 
    n = defaultArg (arg.Name |> Option.map (fun i -> i.Node.Name)) "" )

/// Sub-expression contains variable with given name
let isVariable name = function 
  | Expr.Variable(n) -> n.Node.Name = name | _ -> false

/// Tokenize & parse test code
let parse (code:string) = 
  let code = code.Replace("\r", "").Replace("\n    ","\n")
  let ctx = tokenize code
  code, parseCommands [] ctx, [ for e in ctx.Errors -> e.Number, (e.Range.Start, e.Range.End) ]

/// Format binary operaation for testing purposes
let formatSimpleNumExpr e = 
  let rec loop = function
    | { Node = Expr.Number n } -> string (int n)
    | { Node = Expr.Binary(l, op, r) } -> 
        sprintf "(%s %s %s)" (loop l) (AstOperations.formatToken (TokenKind.Operator op.Node)) (loop r)
    | _ -> "?"
  loop (node { Start = 0; End = 0 } e)

// --------------------------------------------------------------------------------------
// TESTS: Call chains and nesting
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse indented call chain``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
      yadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertErrors []

[<Test>]
let ``Correctly parse aligned call chain without nesting``() =
  let actual = parse """
    foo.
      bar"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors []

[<Test>]
let ``Error reported on identifier following an unfinished chain``() =
  let actual = parse """
    foo.
    bar"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isVariable "bar")
  actual |> assertErrors [203, "bar"]

[<Test>]
let ``Correctly parse indented call chain with nesting``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
          yadda.
      dadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertSubExpr (isProperty "dadda")
  actual |> assertErrors []

[<Test>]
let ``Correctly parse named arguments of method call``() =
  let actual = parse """
    foo(a=bar(b=1, c=2, 3))"""
  actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
  actual |> assertSubExpr (isCall "bar" (hasArgNames ["b"; "c"; ""]))
  actual |> assertErrors []

[<Test>]
let ``Error reported on incomplete named parameter specification``() =
  let actual = parse """
    foo(a=bar(b=, c=2, 3))"""
  actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
  actual |> assertSubExpr (isCall "bar" (hasArgNames [""; "c"; ""]))
  actual |> assertSubExpr (isVariable "b")
  actual |> assertErrors [207, "="]

[<Test>]
let ``Currectly parse nested chain as equality test``() =
  let actual = parse "foo(bar.yadda=1)"
  actual |> assertSubExpr (isCall "foo" (hasArgNames [""]))
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertErrors []

[<Test>]
let ``Error reported on keyword after a call chain``() =
  let actual = parse """
    let a = foo.let"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertErrors [201, "let"]

[<Test>]
let ``Error reported on unfinished call chain``() =
  let actual = parse """
    let a = foo.
      bar.
    let b = a"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors [202, "let"]

[<Test>]
let ``Error reported on unindented token in call chain``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
    yadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isVariable "yadda")
  actual |> assertErrors [203, "yadda"]

// --------------------------------------------------------------------------------------
// TESTS: Call chains with arguments
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse call with arguments``() =
  let actual = parse """
    let a = foo.
      'bar zoo'(1)"""
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))

[<Test>]
let ``Correctly parse multiple nested chain calls`` () =
  let actual = parse """
    let a = foo1
      .foo2(bar1
        .bar2(goo1
          .goo2))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertErrors []

[<Test>]
let ``Error reported on insufficiently indented nested chain`` () =
  let actual = parse """
    let a = foo1
      .foo2(bar1
      .bar2(goo1
      .goo2.'bar zoo'(1)))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))
  actual |> assertErrors [204, "bar1"]

[<Test>]
let ``Correctly parse one inline chain and one indented chain`` () =
  let actual = parse """
    let a = foo1.foo2(bar1.bar2(
      goo1.goo2))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertErrors []


[<Test>]
let ``Report error and stop parsing in unindented argument list`` () =
  let actual = parse """
    let a = foo(
        1,
    bar(2)"""
  actual |> assertErrors [208,","]
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
  actual |> assertSubExpr (isCall "bar" (hasArgValues [isVal 2.0]))

[<Test>]
let ``Report error and continue parsing indented method chain`` () =
  let actual = parse """
    let a = foo(
        1,).yadda
    bar(2)"""
  actual |> assertErrors [207,","]
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
  actual |> assertSubExpr (isProperty "yadda")

[<Test>]
let ``Correctly parse chain with calls and properties`` () = 
  let actual = parse """
    let a = goo.foo("yo").'some bar'
    yadda(2)"""
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal "yo"]))
  actual |> assertSubExpr (isProperty "some bar")
  actual |> assertErrors []

// --------------------------------------------------------------------------------------
// TESTS: List expressions
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse list of inline elements`` () =
  let actual = parse """
    let a = [1, 2, 3]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
  actual |> assertErrors []

[<Test>]
let ``Correctly parse list of elements with line breaks`` () =
  let actual = parse """
    let a = [1, 
      2, 
      3]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
  actual |> assertErrors []

[<Test>]
let ``Report error and stop parsing unindented list`` () =
  let actual = parse """
    let a = [1, 
    foo.bar"""
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors [213, ","]

[<Test>]
let ``Report error and skip over unexpected tokens in list`` () =
  let actual = parse """
    let a = [1,
      2,
      +] 
    foo.bar"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0])
  actual |> assertErrors [212, "+"]

// --------------------------------------------------------------------------------------
// TESTS: Commands
// --------------------------------------------------------------------------------------

[<Test>]
let ``Report error on unifinished let and continue parsing`` () =
  let actual = parse """
    let 
    let b = 1"""
  actual |> assertErrors [215, "let"]
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))

[<Test>]
let ``Report error and treat missing identifier as empty string`` () =
  let actual = parse """
    let 1 + 2
    let b = 1"""
  actual |> assertLet "" (fun e -> equal "(1 + 2)" (formatSimpleNumExpr e); true)
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))
  actual |> assertErrors [214, "1"]

[<Test>]
let ``Report error on let without name or body`` () =
  let actual = parse """
    let +
    let b = 1"""
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))
  actual |> assertErrors [214, "+"]

[<Test>]
let ``Report error on let with name and no equals, but parse it anyway`` () =
  let actual = parse """
    let a foo.bar
    let b = 1"""
  actual |> assertErrors [214, "foo"]
  actual |> assertLet "a" (hasSubExpr (isProperty "bar"))
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))

[<Test>]
let ``Report error on nested expression and parse it as command`` () =
  let actual = parse """
    1 
      3
        4 + 1
    let b = 1"""
  actual |> assertCmds [""; ""; ""; "b"] [any; any; any; any]
  actual |> assertErrors [216, "3"; 216, "4 + 1"]
  actual |> assertSubExpr (isVal 1.0)
  actual |> assertSubExpr (isVal 4.0)

[<Test>]
let ``Report error on nested expression after let and parse it as command`` () =
  let actual = parse """
    let a = 1 
      2
    let b = 3"""
  actual |> assertCmds ["a"; ""; "b"] [hasSubExpr (isVal 1.0); hasSubExpr (isVal 2.0); hasSubExpr (isVal 3.0)]
  actual |> assertErrors [216, "2"]

[<Test>]
let ``Correctly parse mix of let bindings and expression commands`` () =
  let actual = parse """
    let a = 1 + 2 
    3
    let b = 1"""
  actual |> assertCmds ["a";"";"b"] [ any; hasSubExpr (isVal 3.0); any ]
  actual |> assertErrors []

// --------------------------------------------------------------------------------------
// TESTS: Binary operator precedence & parenthesis
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parses precedence of numerical operators`` () =
  let actual = parse "foo(1 + 2 ^ 3 * 4)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "(1 + ((2 ^ 3) * 4))" (formatSimpleNumExpr e)
    true ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parses precedence of Boolean and numerical operators`` () =
  let actual = parse "foo(2 ^ 3 * 4 > 1 + 2 = 1 > 8)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "((((2 ^ 3) * 4) > (1 + 2)) = (1 > 8))" (formatSimpleNumExpr e) 
    true ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parses precedence of operators with nested chain`` () =
  let actual = parse """
    let a = 
      foo(1 + 2 ^ bar.
      yadda * 4)"""
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "(1 + ((2 ^ ?) * 4))" (formatSimpleNumExpr e) 
    true ]))
  actual |> assertErrors [203, "yadda"]

[<Test>]
let ``Correctly parses parenthesized expression`` () =
  let actual = parse "foo(1 + (2 + 3) + 4)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "((1 + (2 + 3)) + 4)" (formatSimpleNumExpr e) 
    true ]))

[<Test>]
let ``Report erroneous token inside parenthesized expression`` () =
  let actual = parse "foo(1 + (2 let) + 4)"
  actual |> assertErrors [209, "let"]

[<Test>]
let ``Report erroneous end of nesting and continue parsing`` () =
  let actual = parse """
    foo(1 + (2
    bar.yadda"""
  actual |> assertErrors [210, "2"; 208, "("]
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertSubExpr (isVariable "bar")

(*

"""
foo.
  bar(baz.
  goo
"""
|> ignore

"""
foo
  .bar(baz
  .goo
"""
|> ignore
*)