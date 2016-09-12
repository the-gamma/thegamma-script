module TheGamma.AstOperations

/// Format a single token
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


let (|ExprLeaf|ExprNode|) e = 
  match e with
  | ExprKind.Property(e, n) -> ExprNode([e], [n])
  | ExprKind.Call(e, n, args) -> ExprNode(e::[for a in args -> a.Value ], n::(args |> List.choose (fun a -> a.Name)))
  | ExprKind.Variable(n) -> ExprNode([], [n])
  | ExprKind.List(els) -> ExprNode(els, [])
  | ExprKind.Function(n, b) -> ExprNode([b], [n])
  | ExprKind.Number _
  | ExprKind.Boolean _
  | ExprKind.String _
  | ExprKind.Unit
  | ExprKind.Null
  | ExprKind.Empty -> ExprLeaf()

let rebuildExprNode e es ns =
  match e, es, ns with
  | ExprKind.List(_), els, [] -> ExprKind.List(els)
  | ExprKind.Function(_), [e], [n] -> ExprKind.Function(n, e)
  | ExprKind.Property(_, _), [e], [n] -> ExprKind.Property(e, n)
  | ExprKind.Call(_, _, args), e::es, n::ns ->
      let rec rebuildArgs args es ns =
        match args, es, ns with
        | { Argument.Name = None }::args, e::es, ns -> { Value = e; Name = None }::(rebuildArgs args es ns)
        | { Argument.Name = Some _ }::args, e::es, n::ns -> { Value = e; Name = Some n }::(rebuildArgs args es ns)
        | [], [], [] -> []
        | _ -> failwith "rebuildExprNode: Wrong call length"
      ExprKind.Call(e, n, rebuildArgs args es ns)
  | ExprKind.Variable _, [], [n] -> ExprKind.Variable(n)
  | ExprKind.Variable _, _, _ -> failwith "rebuildExprNode: Wrong variable length"
  | ExprKind.Property _, _, _ -> failwith "rebuildExprNode: Wrong property length"
  | ExprKind.Call _, _, _ -> failwith "rebuildExprNode: Wrong call length"
  | ExprKind.List _, _, _ -> failwith "rebuildExprNode: Wrong list length"
  | ExprKind.Function _, _, _ -> failwith "rebuildExprNode: Wrong function length"
  | ExprKind.Number _, _, _
  | ExprKind.Boolean _, _, _
  | ExprKind.String _, _, _
  | ExprKind.Empty, _, _ 
  | ExprKind.Null, _, _ 
  | ExprKind.Unit, _, _ -> failwith "rebuildExprNode: Not a node"



module AST2 = 
  open AST2

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
