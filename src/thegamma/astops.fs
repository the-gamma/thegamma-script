module TheGamma.AstOperations

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



