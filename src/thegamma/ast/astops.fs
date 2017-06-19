module TheGamma.Ast

/// Create a node with given range and value
let node rng node =
  { Entity = None
    WhiteBefore = []
    WhiteAfter = [] 
    Node = node
    Range = rng }

/// Does an identifier need escaping?
let needsEscaping (s:string) = 
  (s.[0] >= '0' && s.[0] <= '9') ||
  (s.ToCharArray() |> Array.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) ))

/// Escape identifier if it needs escaping
let escapeIdent s = 
  if s = "" then ""
  elif needsEscaping s then "'" + s + "'" else s

/// Union ranges, assuming Start <= End for each of them
let unionRanges r1 r2 =
  { Start = min r1.Start r2.Start; End = max r1.End r2.End }

/// Is the first range a strict sub-range of the second range
let strictSubRange first second = 
  (first.Start > second.Start && first.End <= second.End) ||
  (first.Start >= second.Start && first.End < second.End)

/// Format a single token (as it looks in the soruce code)
let formatToken = function
  | TokenKind.LParen -> "("
  | TokenKind.RParen -> ")"
  | TokenKind.Equals -> "="
  | TokenKind.Dot -> "."
  | TokenKind.Comma -> ","
  | TokenKind.Colon -> ":"
  | TokenKind.Let -> "let"
  | TokenKind.LSquare -> "["
  | TokenKind.RSquare -> "]"
  | TokenKind.Fun -> "fun"
  | TokenKind.Arrow -> "->"
  | TokenKind.Operator Operator.Modulo -> "%"
  | TokenKind.Operator Operator.Divide -> "/"
  | TokenKind.Operator Operator.GreaterThan -> ">"
  | TokenKind.Operator Operator.GreaterThanOrEqual -> ">="
  | TokenKind.Operator Operator.LessThan -> "<"
  | TokenKind.Operator Operator.LessThanOrEqual -> "<="
  | TokenKind.Operator Operator.Minus -> "-"
  | TokenKind.Operator Operator.Multiply -> "*"
  | TokenKind.Operator Operator.Plus -> "+"
  | TokenKind.Operator Operator.Power -> "^"
  | TokenKind.Operator Operator.Equals -> "="
  | TokenKind.Boolean true -> "true"
  | TokenKind.Boolean false -> "false"
  | TokenKind.Number(s, _) -> s
  | TokenKind.String(s) -> "\"" + s.Replace("\\", "\\\\").Replace("\n", "\\n").Replace("\"", "\\\"") + "\""
  | TokenKind.Ident(i) -> i
  | TokenKind.QIdent(q) -> "'" + q + "'"
  | TokenKind.White(w) -> w
  | TokenKind.Newline -> "\n"
  | TokenKind.Error(c) -> string c
  | TokenKind.EndOfFile -> ""

/// Return human readable description of a token
let formatTokenInfo = function
  | TokenKind.LParen -> "left parenthesis `(`"
  | TokenKind.RParen -> "right parenthesis `)`"
  | TokenKind.Equals -> "equals sign `=`"
  | TokenKind.Dot -> "dot character `.`"
  | TokenKind.Comma -> "comma character `,`"
  | TokenKind.Colon -> "colon character `:`"
  | TokenKind.Let -> "`let` keyword"
  | TokenKind.LSquare -> "left square bracket `[`"
  | TokenKind.RSquare -> "right square bracket `]`"
  | TokenKind.Fun -> "`fun` keyword"
  | TokenKind.Arrow -> "arrow sign `->`"
  | TokenKind.Operator Operator.Equals -> "equals operator `=`"
  | TokenKind.Operator Operator.Divide -> "division sign `/`"
  | TokenKind.Operator Operator.Modulo -> "modulo operator `%`"
  | TokenKind.Operator Operator.GreaterThan -> "greater than sign `>`"
  | TokenKind.Operator Operator.GreaterThanOrEqual -> "greater than or equals sign `>=`"
  | TokenKind.Operator Operator.LessThan -> "less than sign `<`"
  | TokenKind.Operator Operator.LessThanOrEqual -> "less than or equals sign `<=`"
  | TokenKind.Operator Operator.Minus -> "minus sign `-`"
  | TokenKind.Operator Operator.Multiply -> "multiplication sign `*`"
  | TokenKind.Operator Operator.Plus -> "plus sign `+`"
  | TokenKind.Operator Operator.Power -> "exponentiation sign `^`"
  | TokenKind.Boolean true -> "logical `true` value"
  | TokenKind.Boolean false -> "logical `false` value"
  | TokenKind.Number(s, _) -> sprintf "numerical value `%s`" s
  | TokenKind.String(s) -> sprintf "string value `%s`" (s.Replace("`", "'"))
  | TokenKind.Ident(i) -> sprintf "identifer `%s`" i
  | TokenKind.QIdent(q) -> sprintf "quoted identifer `'%s'`" q
  | TokenKind.White(w) -> "whitespace"
  | TokenKind.Newline -> "end of line"
  | TokenKind.Error('`') -> "back-tick character"
  | TokenKind.Error(c) -> sprintf "other character `%s`" (string c)
  | TokenKind.EndOfFile -> "end of file"

/// Turns series of tokens into string, using their Token value
let formatTokens (tokens:seq<Token>) = 
  tokens |> Seq.map (fun t -> formatToken t.Token) |> String.concat ""

/// When formatting expression, we append strings and then join them (should be fast in JS)
type FormattingContext = 
  { Strings : ResizeArray<string> }
  member x.Add(tok) = x.Strings.Add(formatToken tok)

let formatNode (ctx:FormattingContext) f node =
  for t in node.WhiteBefore do ctx.Add(t.Token)
  f ctx node.Node
  for t in node.WhiteAfter do ctx.Add(t.Token)
  
let formatName (ctx:FormattingContext) (name:Name) = 
  if name.Name = "" then ()
  elif needsEscaping name.Name then ctx.Add(TokenKind.QIdent name.Name)
  else ctx.Add(TokenKind.Ident name.Name)

let rec formatArgument (ctx:FormattingContext) (arg:Argument) =
  match arg.Name with 
  | Some name -> 
      formatNode ctx formatName name
      ctx.Add(TokenKind.Equals)
  | _ -> ()
  formatNode ctx formatExpression arg.Value

/// Format a single parsed expression, preserving the parsed whitespace
and formatExpression (ctx:FormattingContext) expr = 
  match expr with
  | Expr.Variable(n) -> 
      formatNode ctx formatName n
  | Expr.Member(inst, mem) -> 
      formatNode ctx formatExpression inst
      ctx.Add(TokenKind.Dot)
      formatNode ctx formatExpression mem
  | Expr.Call(inst, args) ->
      formatNode ctx formatExpression inst
      ctx.Add(TokenKind.LParen)
      args |> formatNode ctx (fun ctx args -> 
        args |> List.iteri (fun i arg ->
          if i <> 0 then ctx.Add(TokenKind.Comma)
          formatArgument ctx arg ) )
      ctx.Add(TokenKind.RParen)
  | Expr.String s -> ctx.Add(TokenKind.String s)
  | Expr.Number n -> ctx.Add(TokenKind.Number(string n, n))
  | Expr.Boolean b -> ctx.Add(TokenKind.Boolean b)
  | Expr.Binary(l, op, r) ->
      formatNode ctx formatExpression l
      op |> formatNode ctx (fun ctx op -> ctx.Add(TokenKind.Operator op))
      formatNode ctx formatExpression r  
  | Expr.Function(n, e) ->
      ctx.Add(TokenKind.Fun)
      formatNode ctx formatName n
      ctx.Add(TokenKind.Arrow)
      formatNode ctx formatExpression e
  | Expr.Placeholder(n, e) ->
      ctx.Add(TokenKind.LSquare)
      formatNode ctx formatName n
      ctx.Add(TokenKind.Colon)
      formatNode ctx formatExpression e
      ctx.Add(TokenKind.RSquare)
  | Expr.List els ->
      ctx.Add(TokenKind.LSquare)
      for e in els do formatNode ctx formatExpression e
      ctx.Add(TokenKind.RSquare)
  | Expr.Empty -> ()

/// Format a single parsed command, preserving the parsed whitespace
let formatCommand (ctx:FormattingContext) cmd = 
  match cmd with
  | Command.Expr e -> 
      formatNode ctx formatExpression e
  | Command.Let(n, e) -> 
      ctx.Add(TokenKind.Let)
      formatNode ctx formatName n
      ctx.Add(TokenKind.Equals)
      formatNode ctx formatExpression e

/// Format single parsed expression, preserving the parsed whitespace
let formatSingleExpression expr = 
  let ctx = { Strings = ResizeArray<_>() }
  formatNode ctx formatExpression expr
  System.String.Concat(ctx.Strings)

/// Format parsed program, preserving the parsed whitespace
let formatProgram (prog:Program) = 
  let ctx = { Strings = ResizeArray<_>() }
  prog.Body |> formatNode ctx (fun ctx cmds ->
    for cmd in cmds do formatNode ctx (formatCommand) cmd)
  System.String.Concat(ctx.Strings)

/// Format all white space after the given expression
let formatWhiteAfterExpr nd = 
  let wa = 
    match nd.Node with 
    | Expr.Variable(n) -> n.WhiteAfter @ nd.WhiteAfter 
    | _ -> nd.WhiteAfter
  String.concat "" [ for t in wa -> formatToken t.Token ]

/// Format all white space before the given expression
let formatWhiteBeforeExpr nd = 
  let wa = 
    match nd.Node with 
    | Expr.Variable(n) -> nd.WhiteBefore @ n.WhiteBefore 
    | Expr.Member(_, m & { Node = Expr.Variable n }) -> nd.WhiteBefore @ m.WhiteBefore @ n.WhiteBefore
    | _ -> nd.WhiteBefore
  String.concat "" [ for t in wa -> formatToken t.Token ]

/// Format entity kind into something readable
let formatEntityKind = function
  | EntityKind.GlobalValue _ -> "global value"
  | EntityKind.Variable _ -> "variable"
  | EntityKind.Binding _ -> "binding"
  | EntityKind.Operator(_, op, _) -> (formatToken (TokenKind.Operator op)) + " operator"
  | EntityKind.List _ -> "list"
  | EntityKind.Constant(Constant.Empty) -> "empty value"
  | EntityKind.Constant(Constant.Number n) -> sprintf "number `%f`" n 
  | EntityKind.Constant(Constant.String n) -> sprintf "string `%s`" n 
  | EntityKind.Constant(Constant.Boolean true) -> "`true` value" 
  | EntityKind.Constant(Constant.Boolean false) -> "`false` value" 
  | EntityKind.Function _ -> "function"
  | EntityKind.LetCommand _ -> "let command"
  | EntityKind.RunCommand _ -> "run command"
  | EntityKind.Program _ -> "program"
  | EntityKind.Root _ -> "root"
  | EntityKind.CallSite _ -> "call site"
  | EntityKind.NamedParam _ -> "named param"
  | EntityKind.Call _ -> "call"
  | EntityKind.ArgumentList _ -> "argument list"
  | EntityKind.Member _ -> "member access"
  | EntityKind.MemberName _ -> "member name"
  | EntityKind.Placeholder _ -> "placeholder"

/// Return entity name (or anonymous) and all its antecedants
let rec entityCodeNameAndAntecedents = function
  | EntityKind.Root -> 0, [], "<root>"
  | EntityKind.Program(ans) -> 1, ans, "<program>"
  | EntityKind.RunCommand(an) -> 2, [an], "<do>"
  | EntityKind.LetCommand(an1, an2) -> 3, [an1; an2], "<let>"
  | EntityKind.Operator(an1, op, an2) -> 4, [an1; an2], (formatToken (TokenKind.Operator op))
  | EntityKind.List(ans) -> 5, ans, "<list>"
  | EntityKind.Constant(Constant.String s) -> 6, [], s
  | EntityKind.Constant(Constant.Number n) -> 7, [], (string n)
  | EntityKind.Constant(Constant.Boolean b) -> 8, [], (string b)
  | EntityKind.Constant(Constant.Empty) -> 9, [], "<empty>"
  | EntityKind.Function(an1, an2) -> 10, [an1; an2], "<function>"
  | EntityKind.GlobalValue(n, _) -> 11, [], n.Name
  | EntityKind.Variable(n, an) -> 12, [an], n.Name
  | EntityKind.Binding(n, an) -> 13, [an], n.Name
  | EntityKind.ArgumentList(ans) -> 14, ans, "<args>"
  | EntityKind.Call(an1, an2) -> 15, [an1; an2], "<call>"
  | EntityKind.Member(an1, an2) -> 16, [an1; an2], "<member>"
  | EntityKind.NamedParam(n, an) -> 17, [an], n.Name
  | EntityKind.Placeholder(n, an) -> 18, [an], n.Name
  | EntityKind.CallSite(an, Choice1Of2 s) -> 19, [an], s
  | EntityKind.CallSite(an, Choice2Of2 m) -> 20, [an], string m
  | EntityKind.MemberName(n) -> 21, [], n.Name

/// Return the entity representing the name just before call in call chain
let rec lastChainElement ent = 
  match ent.Kind with
  | EntityKind.Variable _ -> ent
  | EntityKind.Member(_, mem) -> mem
  | _ -> ent

// Provide easy access to entity's antecedents
type Entity with
  member x.Antecedents = let _, ans, _ = entityCodeNameAndAntecedents x.Kind in ans
  member x.Name = let _, _, name = entityCodeNameAndAntecedents x.Kind in name

/// Return full name of the type
let rec formatType = function
  | Type.Delayed _ -> "delayed type"
  | Type.Primitive PrimitiveType.Bool -> "bool"
  | Type.Primitive PrimitiveType.Date -> "date"
  | Type.Primitive PrimitiveType.Number -> "number"
  | Type.Primitive PrimitiveType.String -> "string"
  | Type.Primitive PrimitiveType.Unit -> "unit"
  | Type.Object obj ->  
      try 
        let mem = obj.Members
        let mems = mem |> Seq.truncate 5 |> Seq.map (fun m -> m.Name) |> String.concat ", "
        "{ " + if mem.Length > 5 then mems + ", ..." else mems + " }"
      with _ -> "{ members }"
  | Type.Method(tin, tout) -> 
      let tout = match tout [for ma in tin -> ma.Type, None ] with Some t -> formatType t | _ -> "?"
      let tin = 
        [ for ma in tin -> 
            sprintf "%s%s%s:%s" (if ma.Optional then "?" else "") 
              (if ma.Static then "!" else "") ma.Name (formatType ma.Type) ]
        |> String.concat ", " 
      "(" + tin + ") -> " + tout
  | Type.List t -> "list<" + formatType t + ">"
  | Type.Any -> "any"

/// Return readable name of the top-level node in the type
let formatTypeInfo = function
  | Type.Delayed _ -> "delayed type"
  | Type.Primitive PrimitiveType.Bool -> "bool"
  | Type.Primitive PrimitiveType.Date -> "date"
  | Type.Primitive PrimitiveType.Number -> "number"
  | Type.Primitive PrimitiveType.String -> "string"
  | Type.Primitive PrimitiveType.Unit -> "unit"
  | Type.Object _ -> "object type"
  | Type.Method _ -> "method type"
  | Type.List _ -> "list type"
  | Type.Any _ -> "unknown"

/// When pattern matching using `ExprNode`, this function lets you rebuild
/// the original node from the original expression, new expressions & names
let rebuildExprNode e es ns =
  match e, es, ns with
  | Expr.Placeholder(_, _), [e], [n] -> Expr.Placeholder(n, e)
  | Expr.List(_), els, [] -> Expr.List(els)
  | Expr.Function(_), [e], [n] -> Expr.Function(n, e)
  | Expr.Member(_, _), [e1; e2], [] -> Expr.Member(e1, e2)
  | Expr.Binary(_, op, _), [e1; e2], [] -> Expr.Binary(e1, op, e2)
  | Expr.Call(_, args), e::es, ns ->
      let rec rebuildArgs args es ns =
        match args, es, ns with
        | { Argument.Name = None }::args, e::es, ns -> { Value = e; Name = None }::(rebuildArgs args es ns)
        | { Argument.Name = Some _ }::args, e::es, n::ns -> { Value = e; Name = Some n }::(rebuildArgs args es ns)
        | [], [], [] -> []
        | _ -> failwith "rebuildExprNode: Wrong call length"
      Expr.Call(e, { args with Node = rebuildArgs args.Node es ns })
  | Expr.Variable _, [], [n] -> Expr.Variable(n)
  | Expr.Placeholder _, _, _ -> failwith "rebuildExprNode: Wrong placeholder length"
  | Expr.Variable _, _, _ -> failwith "rebuildExprNode: Wrong variable length"
  | Expr.Member _, _, _ -> failwith "rebuildExprNode: Wrong member length"
  | Expr.Call _, _, _ -> failwith "rebuildExprNode: Wrong call length"
  | Expr.List _, _, _ -> failwith "rebuildExprNode: Wrong list length"
  | Expr.Function _, _, _ -> failwith "rebuildExprNode: Wrong function length"
  | Expr.Binary _, _, _ -> failwith "rebuildExprNode: Wrong binary operator argument length"
  | Expr.Number _, _, _
  | Expr.Boolean _, _, _
  | Expr.String _, _, _
  | Expr.Empty, _, _ -> failwith "rebuildExprNode: Not a node"

/// ExprNode matches when an expression contains nested expressions or names,
/// ExprLeaf matches when an expression is a primitive (number, bool, etc..)
let (|ExprLeaf|ExprNode|) e = 
  match e with
  | Expr.Placeholder(n, e) -> ExprNode([e], [n])
  | Expr.Member(e1, e2) -> ExprNode([e1; e2], [])
  | Expr.Call(e, args) -> ExprNode(e::[for a in args.Node -> a.Value ], (args.Node |> List.choose (fun a -> a.Name)))
  | Expr.Variable(n) -> ExprNode([], [n])
  | Expr.List(els) -> ExprNode(els, [])
  | Expr.Function(n, b) -> ExprNode([b], [n])
  | Expr.Binary(l, op, r) -> ExprNode([l; r], [])
  | Expr.Number _
  | Expr.Boolean _
  | Expr.String _
  | Expr.Empty -> ExprLeaf()

/// Find object member with the specified name 
let (|FindMember|_|) (name:Name) (obj:ObjectType) = 
  obj.Members |> Seq.tryPick (fun m -> if m.Name = name.Name then Some(m) else None) 
