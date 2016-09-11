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
// 
// --------------------------------------------------------------------------------------


let unionRanges r1 r2 =
  { Start = min r1.Start r2.Start
    End = max r1.End r2.End }


type Context = 
  { Tokens : Token[]
    Whitespace : ResizeArray<Token>
    Errors : ResizeArray<Error<Range>>
    mutable IndentCurrent : int
    mutable IndentStack : (int * bool) list
    mutable Position : int }

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

let endIndent ctx = 
  match ctx.IndentStack with
  | t::stack -> ctx.IndentStack <- stack
  | _ -> failwith "error - endIndent when stack is empty"

let startIndent ctx =
  match ctx.IndentStack with
  | (prev, true)::_ when prev >= ctx.IndentCurrent -> false
  | (prev, false)::_ when prev <> ctx.IndentCurrent -> failwith "TODO: startIndent - forgot to initialize line"
  | _ ->
      ctx.IndentStack <- (ctx.IndentCurrent, false)::ctx.IndentStack
      true

let addError ctx e = ctx.Errors.Add(e)

let current ctx = ctx.Tokens.[ctx.Position]

let rec token ctx = 
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
      token ctx
  | { Token = TokenKind.White _ } as t ->
      addWhitespace ctx t
      next ctx
      token ctx
  | t -> 
      getWhitespace ctx, t
      
let nestedToken ctx = 
  let t = token ctx
  match ctx.IndentStack with
  | (indent, _)::_ when ctx.IndentCurrent >= indent -> Some t
  | [] -> failwith "TODO: Should not check nestedToken with no indent stack"
  | _ -> None

let node rng n = 
  { Node = n; Range = rng; WhiteBefore = []; WhiteAfter = [] }

let whiteAfter w n = { n with WhiteAfter = w }

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



/// Property access or method call after '.' in a nested block
let rec parseChain silentErrors optInst previd ctx = 
  let inst = 
    match optInst with
    | Some inst -> node (unionRanges inst.Range previd.Range) (Expr.Property(inst, previd))
    | None -> node previd.Range (Expr.Variable(previd))
  
  match nestedToken ctx with
  | Some (Identifier id) ->
      next ctx
      parseMember (Some inst) id ctx

  | Some(_, t) ->      
      // Error: Expected identifier (after '.') but there was some other token
      // at the correct level of nesting, so skip it & try prsing next thing
      if not silentErrors then Errors.Parser.unexpectedTokenAfterDot t.Range t.Token |> addError ctx 
      if t.Token = TokenKind.EndOfFile then inst
      else 
        next ctx
        parseChain true optInst previd ctx
      
  | None ->
  match token ctx with
  | Identifier id ->
      // Error: There is an identifier after '.' but it is not properly indented
      let rng = lastCallOrPropertyRange inst
      Errors.Parser.unindentedIdentifierAfterDot id.Range rng id.Node.Name |> addError ctx 
      inst

  | _, t ->      
      // Error: Expected more after '.' but the nested scope ends here
      // Just return what we got so far & end the current chain
      let rng = lastCallOrPropertyRange inst
      Errors.Parser.unexpectedScopeEndAfterDot t.Range rng t.Token |> addError ctx 
      inst

/// Call chain after name - either '.' & more or '(...)' or end of call chain
and parseMember (optInst:option<_>) (id:Node<Name>) ctx : Node<_> = 
  match nestedToken ctx with
  | Some(white, ({ Token = TokenKind.LParen } as startTok)) ->
      next ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      let endRange, white, args = parseExpressionList [] ctx
      let args = node (unionRanges startTok.Range endRange) args
      let erng = defaultArg (Option.map (fun n -> n.Range) optInst) id.Range
      node (unionRanges erng args.Range) (Expr.Call(optInst, id, args))

  | Some(white, { Token = TokenKind.Dot }) ->
      next ctx
      let optInst = optInst |> Option.map (whiteAfter white)
      parseChain false optInst id ctx

  | _ -> 
      match optInst with
      | Some inst -> node (unionRanges inst.Range id.Range) (Expr.Property(inst, id))
      | None -> node id.Range (Expr.Variable(id))


/// A term is a single thing inside expression involving operators, i.e.
///   <expression> := <term> <op> <term> <op> .. <op> <term>
and parseTerm ctx = 
  match token ctx with
  // Variable or call chain
  | Identifier id ->
      next ctx
      if not (startIndent ctx) then failwith "TODO: Cannot start indent"
      let varOrCall = parseMember None id ctx 
      endIndent ctx
      Some varOrCall

  // String, numeric and Boolean literals
  | white, { Token = TokenKind.Number(_, n); Range = r }  ->
      next ctx
      node r (Expr.Number n) |> whiteAfter white |> Some
  | white, { Token = TokenKind.String(s); Range = r }  ->
      next ctx
      node r (Expr.String s) |> whiteAfter white |> Some
  | white, { Token = TokenKind.Boolean(b); Range = r }  ->
      next ctx
      node r (Expr.Boolean b) |> whiteAfter white |> Some

  // Not a term, but that's fine
  | _ -> None

and parseExpression ctx = 
  match parseTerm ctx with
  | Some t -> Some t
  | None -> None      

and parseExpressionList acc ctx = 
  let parsed, acc = 
    // TODO: Named arguments
    match parseExpression ctx with
    | None -> false, acc
    | Some e -> true, { Name = None; Value = e }::acc

  match token ctx with
  | white, ({ Token = TokenKind.RParen } as t) ->
      next ctx
      t.Range, white, acc

  | white, { Token = TokenKind.Comma } when parsed ->
      next ctx
      parseExpressionList acc ctx

  | _, t ->
      failwithf "Expected ) or , (if parsed=true) but got %A" t.Token


let rec parseCommand ctx = 
  match token ctx with
  | whiteBeforeLet, { Token = TokenKind.Let; Range = rngLet } ->
      next ctx
      if not (startIndent ctx) then failwith "TODO: Cannot start indent"
      match nestedToken ctx with
      | Some(Identifier id) ->
          next ctx
          match nestedToken ctx with
          | Some (whiteAfterId, { Token = TokenKind.Equals }) ->
              next ctx
              match parseExpression ctx with
              | Some body ->
                { Node = Command.Let({ id with WhiteAfter = whiteAfterId }, body)
                  WhiteBefore = whiteBeforeLet; WhiteAfter = []
                  Range = unionRanges rngLet body.Range }
              | None ->
                  failwith "not expression"
          | Some t ->
              failwith "missing equals after let ident"
          | None ->
              failwith "unfinished let (missing body)"
      | Some _ ->
          failwith "missing ident after let"
      | None ->
          failwith "unfinished let"
  | t ->
      match parseExpression ctx with
      | Some expr ->
          { WhiteBefore = []; WhiteAfter = []; Range = expr.Range
            Node = Command.Expr expr }
      | None ->
          failwith (sprintf "Not expression: %A" t)

let rec parseCommands acc ctx = 
  match token ctx with
  | white, { Token = TokenKind.EndOfFile } ->
      match acc with 
      | x::xs -> List.rev ({ x with WhiteAfter = white }::xs)
      | [] -> []
  | _ ->
      let cmd = parseCommand ctx
      parseCommands (cmd::acc) ctx

let tokenize input = 
  { Tokens = Tokenizer.tokenize input |> fst
    Position = 0
    IndentCurrent = 0
    IndentStack = []
    Errors = ResizeArray<_>()
    Whitespace = ResizeArray<_>() }

//let ctx = tokenize "\nlet a = foo.\n      'bar zoo'.\n  yadda"
//parseCommand ctx



// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Assert that result contains given errors
let assertErrors expectErrors ((code:string), cmds, errs) = 
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, (astart, aend)) in List.zip expectErrors errs do
    let s = code.Substring(astart, aend-astart+1)
    equal en an
    equal ec s

/// Assert that result contains given sub-expression
let assertSubExpr f (code, (cmds:Node<Command> list), errs) = 
  let rec loop e = 
    if f e then true else
      match e with 
      | ExprNode(es, _) -> es |> List.exists (fun e -> loop e.Node)
      | ExprLeaf _ -> false
  let matches = 
    cmds |> List.exists (fun cmd ->
      match cmd.Node with
      | Command.Expr e -> loop e.Node
      | Command.Let(_, e) -> loop e.Node )
  equal true matches

/// Sub-expression contains property with given name
let isProperty name = function 
  | Expr.Property(_, n) -> n.Node.Name = name | _ -> false

/// Sub-expression contains call with given name and arguments match function
let isCall name ac = function 
  | Expr.Call(_, n, args) -> n.Node.Name = name && ac args | _ -> false

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

/// Sub-expression contains variable with given name
let isVariable name = function 
  | Expr.Variable(n) -> n.Node.Name = name | _ -> false

/// Tokenize & parse test code
let parse (code:string) = 
  let code = code.Replace("\r", "").Replace("\n    ","\n")
  let ctx = tokenize code
  code, parseCommands [] ctx, [ for e in ctx.Errors -> e.Number, (e.Range.Start, e.Range.End) ]


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
  actual |> assertErrors [23, "bar"]

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
let ``Error reported on keyword after a call chain``() =
  let actual = parse """
    let a = foo.let"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertErrors [21, "let"]

[<Test>]
let ``Error reported on unfinished call chain``() =
  let actual = parse """
    let a = foo.
      bar.
    let b = a"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors [22, "let"]

[<Test>]
let ``Error reported on unindented token in call chain``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
    yadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isVariable "yadda")
  actual |> assertErrors [23, "yadda"]

[<Test>]
let ``Correctly parse call with arguments``() =
  let actual = parse """
    let a = foo.
      'bar zoo'(1)"""
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1]))

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