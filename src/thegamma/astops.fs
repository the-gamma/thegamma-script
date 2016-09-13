module TheGamma.Ast

/// Create a node with given range and value
let node rng node =
  { WhiteBefore = []
    WhiteAfter = [] 
    Node = node
    Range = rng }

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
  | TokenKind.By | TokenKind.To -> failwith "Unsupported token"

/// Return human readable description of a token
let formatTokenInfo = function
  | TokenKind.LParen -> "left parenthesis `(`"
  | TokenKind.RParen -> "right parenthesis `)`"
  | TokenKind.Equals -> "equals sign `=`"
  | TokenKind.Dot -> "dot character `.`"
  | TokenKind.Comma -> "comma character `,`"
  | TokenKind.Let -> "`let` keyword"
  | TokenKind.LSquare -> "left square bracket `[`"
  | TokenKind.RSquare -> "right square bracket `]`"
  | TokenKind.Fun -> "`fun` keyword"
  | TokenKind.Arrow -> "arrow sign `->`"
  | TokenKind.Operator Operator.Equals -> "equals operator `=`"
  | TokenKind.Operator Operator.Divide -> "division sign `/`"
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
  | TokenKind.By | TokenKind.To -> failwith "Unsupported token"

/// Turns series of tokens into string, using their Token value
let formatTokens (tokens:seq<Token>) = 
  tokens |> Seq.map (fun t -> formatToken t.Token) |> String.concat ""


/// When pattern matching using `ExprNode`, this function lets you rebuild
/// the original node from the original expression, new expressions & names
let rebuildExprNode e es ns =
  match e, es, ns with
  | Expr.List(_), els, [] -> Expr.List(els)
  | Expr.Function(_), [e], [n] -> Expr.Function(n, e)
  | Expr.Property(_, _), [e], [n] -> Expr.Property(e, n)
  | Expr.Binary(_, op, _), [e1; e2], [] -> Expr.Binary(e1, op, e2)
  | Expr.Call(inst, _, args), e::es, n::ns ->
      let e, es = if inst.IsSome then Some e, es else None, e::es
      let rec rebuildArgs args es ns =
        match args, es, ns with
        | { Argument.Name = None }::args, e::es, ns -> { Value = e; Name = None }::(rebuildArgs args es ns)
        | { Argument.Name = Some _ }::args, e::es, n::ns -> { Value = e; Name = Some n }::(rebuildArgs args es ns)
        | [], [], [] -> []
        | _ -> failwith "rebuildExprNode: Wrong call length"
      Expr.Call(e, n, { args with Node = rebuildArgs args.Node es ns })
  | Expr.Variable _, [], [n] -> Expr.Variable(n)
  | Expr.Variable _, _, _ -> failwith "rebuildExprNode: Wrong variable length"
  | Expr.Property _, _, _ -> failwith "rebuildExprNode: Wrong property length"
  | Expr.Call _, _, _ -> failwith "rebuildExprNode: Wrong call length"
  | Expr.List _, _, _ -> failwith "rebuildExprNode: Wrong list length"
  | Expr.Function _, _, _ -> failwith "rebuildExprNode: Wrong function length"
  | Expr.Binary _, _, _ -> failwith "rebuildExprNode: Wrong binary operator argument length"
  | Expr.Number _, _, _
  | Expr.Boolean _, _, _
  | Expr.String _, _, _
  | Expr.Empty, _, _ 
  | Expr.Null, _, _ 
  | Expr.Unit, _, _ -> failwith "rebuildExprNode: Not a node"

/// ExprNode matches when an expression contains nested expressions or names,
/// ExprLeaf matches when an expression is a primitive (number, bool, etc..)
let (|ExprLeaf|ExprNode|) e = 
  match e with
  | Expr.Property(e, n) -> ExprNode([e], [n])
  | Expr.Call(Some e, n, args) -> ExprNode(e::[for a in args.Node -> a.Value ], n::(args.Node |> List.choose (fun a -> a.Name)))
  | Expr.Call(None, n, args) -> ExprNode([for a in args.Node -> a.Value ], n::(args.Node |> List.choose (fun a -> a.Name)))
  | Expr.Variable(n) -> ExprNode([], [n])
  | Expr.List(els) -> ExprNode(els, [])
  | Expr.Function(n, b) -> ExprNode([b], [n])
  | Expr.Binary(l, op, r) -> ExprNode([l; r], [])
  | Expr.Number _
  | Expr.Boolean _
  | Expr.String _
  | Expr.Unit
  | Expr.Null
  | Expr.Empty -> ExprLeaf()
