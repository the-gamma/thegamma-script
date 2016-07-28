module TheGamma.AstOperations

let (|ExprLeaf|ExprNode|) e = 
  match e with
  | ExprKind.Property(e, n) -> ExprNode([e], [n])
  | ExprKind.Call(e, n, args) -> ExprNode(e::[for a in args -> a.Value ], n::(args |> List.choose (fun a -> a.Name)))
  | ExprKind.Variable(n) -> ExprNode([], [n])
  | ExprKind.Number _
  | ExprKind.Boolean _
  | ExprKind.Unit
  | ExprKind.Empty -> ExprLeaf()

let rebuildExprNode e es ns =
  match e, es, ns with
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
  | ExprKind.Number _, _, _
  | ExprKind.Boolean _, _, _
  | ExprKind.Empty, _, _ 
  | ExprKind.Unit, _, _ -> failwith "rebuildExprNode: Not a node"



