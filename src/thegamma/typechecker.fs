module TheGamma.TypeChecker

open TheGamma
open Fable.Extensions

type CheckingContext = 
  { Variables : Map<string, Type> }

type CheckingResult = 
  { Errors : Error list }

let addError e ctx = { ctx with Errors = e::ctx.Errors }
let addVariable k v ctx = { ctx with Variables = Map.add k v ctx.Variables }

let awaitFuture (f:Future<'T>) = Async.FromContinuations(fun (cont, _, _) ->
  f.Then(cont))

type ObjectMembers = 
  | Members of Member list
  | NotAnObject
  | SilentError

let rec getObjectMembers t = async {
  match t with
  | Type.Delayed f ->
      let! t = awaitFuture f
      return! getObjectMembers t 
  | Type.Object o -> return Members(o.Members)
  | Type.Unit
  | Type.Primitive _ -> return NotAnObject
  | Type.Any _ -> return SilentError }

let rec asyncFoldMap f st l = async {
  match l with
  | x::xs ->
      let! st, y = f st x
      let! st, ys = asyncFoldMap f st xs
      return st, y::ys
  | [] -> return st, [] }

let rec asyncFold f st l = async {
  match l with
  | x::xs ->
      let! st = f st x
      return! asyncFold f st xs 
  | [] -> return st }

let rec typeCheckExpr ctx res (expr:Expr<unit>) = async {
  match expr.Expr with
  | ExprKind.Unit ->
      return { Expr = ExprKind.Unit; Type = Type.Unit; Range = expr.Range }, res
  | ExprKind.Empty ->
      return { Expr = ExprKind.Empty; Type = Type.Any; Range = expr.Range }, res
  | ExprKind.Variable(v) when ctx.Variables.ContainsKey v.Name ->  
      return { Expr = ExprKind.Variable v; Type = ctx.Variables.[v.Name]; Range = expr.Range }, res
  | ExprKind.Variable(v) ->
      return 
        { Expr = ExprKind.Variable v; Type = Type.Any; Range = expr.Range }, 
        res |> addError (Errors.TypeChecker.variableNotInScope expr.Range v.Name)
  | ExprKind.Number n -> 
      return { Expr = ExprKind.Number n; Type = Type.Primitive "num"; Range = expr.Range }, res
  | ExprKind.Boolean b -> 
      return { Expr = ExprKind.Boolean b; Type = Type.Primitive "bool"; Range = expr.Range }, res

  | ExprKind.Property(e, name) ->
      let! typed, res = typeCheckExpr ctx res e
      let! members = getObjectMembers typed.Type 
      let resTyp, res = 
        match members with
        | ObjectMembers.Members members ->
            match members |> Seq.tryPick (function Member.Property(n, r) when n = name.Name -> Some r | _ -> None) with
            | Some resTyp -> resTyp, res
            | _ -> Type.Any, res |> addError (Errors.TypeChecker.propertyMissing name.Range name.Name members)
        | ObjectMembers.SilentError -> Type.Any, res
        | ObjectMembers.NotAnObject -> Type.Any, res |> addError (Errors.TypeChecker.notAnObject e.Range typed.Type)
      return { Expr = ExprKind.Property(typed, name); Type = resTyp; Range = expr.Range }, res

  | ExprKind.Call(e, name, args) ->
      let! typed, res = typeCheckExpr ctx res e
      let! members = getObjectMembers typed.Type 

      let! res, typedArgs = 
        args |> asyncFoldMap (fun res arg -> async {
          let! t, res = typeCheckExpr ctx res arg.Value
          return res, { Name = arg.Name; Value = t } }) res

      let resTyp, res = 
        match members with
        | ObjectMembers.Members members ->
            match members |> Seq.tryPick (function Member.Method(n, args, r) when n = name.Name -> Some(r, args) | _ -> None) with
            | Some(resTyp, args) -> 
                // TODO: check arguments
                resTyp, res
            | _ -> Type.Any, res |> addError (Errors.TypeChecker.methodMissing name.Range name.Name members)
        | ObjectMembers.SilentError -> Type.Any, res
        | ObjectMembers.NotAnObject -> Type.Any, res |> addError (Errors.TypeChecker.notAnObject e.Range typed.Type)

      return { Expr = ExprKind.Call(typed, name, typedArgs); Type = resTyp; Range = expr.Range }, res }

let rec typeCheckCmd ctx res cmds = async {
  match cmds with
  | ({ Command = CommandKind.Let(name, expr) } as cmd)::cmds ->
      let! typed, res = typeCheckExpr ctx res expr
      let! cmds, res = typeCheckCmd (addVariable name.Name typed.Type ctx) res cmds
      return { Command = CommandKind.Let(name, typed); Range = cmd.Range}::cmds, res
  | ({ Command = CommandKind.Expr(expr) } as cmd)::cmds ->      
      let! typed, res = typeCheckExpr ctx res expr
      let! cmds, res = typeCheckCmd ctx res cmds
      return { Command = CommandKind.Expr(typed); Range = cmd.Range}::cmds, res
  | [] -> 
      return [], res }

type EditorInfo = 
  { Completions : list<Range * Member list> }

let withCompletion r t ctx = async {
  let! members = getObjectMembers t
  match members with
  | Members members -> return { ctx with Completions = (r, members)::ctx.Completions }
  | _ -> return ctx }

let rec collectExprInfo ctx expr = async {
  match expr.Expr with
  | ExprKind.Property(inst, n) -> 
      let! ctx = collectExprInfo ctx inst
      return! withCompletion n.Range inst.Type ctx
  | ExprKind.Call(inst, n, args) ->
      let! ctx = collectExprInfo ctx inst
      let! ctx = args |> asyncFold (fun ctx arg -> collectExprInfo ctx arg.Value) ctx
      return! withCompletion n.Range inst.Type ctx 
  | ExprKind.Empty
  | ExprKind.Unit
  | ExprKind.Number _
  | ExprKind.Boolean _
  | ExprKind.Variable _ -> return ctx }

let rec collectCmdInfo ctx cmds = async {
  match cmds with
  | [] -> return ctx
  | { Command = CommandKind.Let(_, expr) }::cmds 
  | { Command = CommandKind.Expr(expr) }::cmds ->
      let! ctx = collectExprInfo ctx expr
      return! collectCmdInfo ctx cmds }