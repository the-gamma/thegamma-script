module TheGamma.TypeChecker

open TheGamma

type CheckingContext = 
  { Globals : Map<string, Type> }

let awaitFuture (f:Future<'T>) = Async.FromContinuations(fun (cont, _, _) ->
  f.Then(cont))

let rec asObjectType t = async {
  match t with
  | Type.Delayed f ->
      let! t = awaitFuture f
      return! asObjectType t 
  | Type.Object o -> return o
  | Type.Primitive _ -> return failwith "Expected object type" }

let rec asyncMap f l = async {
  match l with
  | x::xs ->
      let! y = f x
      let! ys = asyncMap f xs
      return y::ys
  | [] -> return [] }

let rec typeCheck ctx (expr:Expr<unit>) = async {
  match expr.Expr with
  | ExprKind.Variable(v) when ctx.Globals.ContainsKey v ->  
      return { Expr = ExprKind.Variable v; Type = ctx.Globals.[v]; Range = expr.Range }
  | ExprKind.Variable(v) ->
      return failwith (sprintf "Variable '%s' is not global" v)
  | ExprKind.Number n -> 
      return { Expr = ExprKind.Number n; Type = Type.Primitive "num"; Range = expr.Range }
  | ExprKind.Boolean b -> 
      return { Expr = ExprKind.Boolean b; Type = Type.Primitive "bool"; Range = expr.Range }
  | ExprKind.Property(e, name) ->
      let! typed = typeCheck ctx e
      let! objTyp = asObjectType typed.Type 
      let resTypOpt = objTyp.Members |> Seq.tryPick (function Member.Property(n, r) when n = name -> Some r | _ -> None)
      let resTyp = 
        match resTypOpt with
        | Some(resTyp) -> resTyp
        | _ -> failwith (sprintf "Could not find property '%s' in '%A'" name objTyp.Members)
      return { Expr = ExprKind.Property(typed, name); Type = resTyp; Range = expr.Range }

  | ExprKind.Call(e, name, args) ->
      let! typed = typeCheck ctx e
      let! objTyp = asObjectType typed.Type 
      let callOpt = objTyp.Members |> Seq.tryPick (function Member.Method(n, r, args) when n = name -> Some(r, args) | _ -> None)
      let methArgs, resTyp = 
        match callOpt with
        | Some(methArgs, resTyp) -> methArgs, resTyp
        | _ -> failwith (sprintf "Could not find method '%s' in '%A'" name objTyp.Members)

      let! typedArgs = 
        args |> asyncMap (fun arg -> async {
          let! t = typeCheck ctx arg.Value
          return { Name = arg.Name; Value = t } })
      // TODO: Check arguments

      return { Expr = ExprKind.Call(typed, name, typedArgs); Type = resTyp; Range = expr.Range } }




let subExpressions expr = 
  match expr.Expr with
  | ExprKind.Property(e, _) -> [e]
  | ExprKind.Call(e1, _, args) -> e1 :: (List.map (fun a -> a.Value) args)
  | ExprKind.Number _
  | ExprKind.Boolean _
  | ExprKind.Variable _ -> []

let rec typeBefore loc best expr = 
  let sub = subExpressions expr
  let best = 
    expr::sub
    |> List.filter (fun e -> e.Range.End <= loc)
    |> List.fold (fun best e ->   
        match best with
        | None -> Some e
        | Some best when e.Range.End > best.Range.End -> Some e 
        | best -> best ) best
  
  sub
  |> List.filter (fun e -> e.Range.Start <= loc && e.Range.End >= loc)
  |> List.fold (fun best e -> typeBefore loc best e) best

