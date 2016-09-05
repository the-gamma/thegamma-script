module TheGamma.TypeChecker

open TheGamma
open Fable.Extensions

// ------------------------------------------------------------------------------------------------
// Type checking
// ------------------------------------------------------------------------------------------------

type CheckingContext = 
  { Variables : Map<string, Type> }

type CheckingResult = 
  { Errors : Error<Range> list }

let addError e ctx = { ctx with Errors = e::ctx.Errors }
let addVariable k v ctx = { ctx with Variables = Map.add k v ctx.Variables }

let rec listsEqual l1 l2 f = 
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys when f x y -> listsEqual xs ys f
  | _ -> false 

let rec arraysEqual (l1:_[]) (l2:_[]) f = 
  let rec loop i =
    if i = l1.Length && i = l2.Length then true
    elif i < l1.Length && i < l2.Length then 
      f (l1.[i]) (l2.[i]) && loop (i+1)
    else false
  loop 0

let rec membersEqual m1 m2 = // Ignoring types, because generics make that hard
  match m1, m2 with 
  | Member.Property(n1, t1, s1, d1, _), Member.Property(n2, t2, s2, d2, _) -> n1 = n2 && s1 = s2 && d1 = d2 // && typesEqual t1 t2
  | Member.Method(n1, t1, a1, r1, d1, _), Member.Method(n2, t2, a2, r2, d2, _) -> 
      n1 = n2 && d1 = d2 && // t1 = t2 && typesEqual r1 r2 && 
        listsEqual a1 a2 (fun (s1, b1, t1) (s2, b2, t2) -> 
          s1 = s2 && b1 = b2(* && typesEqual t1 t2 *) )
  | _ -> false

and typesEqual t1 t2 = 
  match t1, t2 with
  | Type.Any, Type.Any -> true
  | Type.Delayed(g1, _), Type.Delayed(g2, _) -> g1 = g2
  | Type.Function(a1, r1), Type.Function(a2, r2) -> 
      listsEqual (r1::a1) (r2::a2) typesEqual
  | Type.Object(o1), Type.Object(o2) -> 
      listsEqual o1.Typeargs o2.Typeargs typesEqual &&
      arraysEqual o1.Members o2.Members membersEqual
  | Type.Parameter n1, Type.Parameter n2 -> n1 = n2
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2
  | Type.Unit, Type.Unit -> true
  | _ -> false

type ObjectMembers = 
  | Members of Member[]
  | NotAnObject
  | SilentError

let rec getObjectMembers t = async {
  match t with
  | Type.Delayed(_, f) ->
      let! t = Async.AwaitFuture f
      return! getObjectMembers t 
  | Type.Object(o) -> return Members(o.Members)
  | Type.Unit
  | Type.Parameter _
  | Type.Function _
  | Type.List _
  | Type.Primitive _ -> return NotAnObject
  | Type.Any _ -> return SilentError }

let rec unifyTypes assigns ts1 ts2 =
  match ts1, ts2 with
  | Type.Delayed(id1, f1)::ts1, Type.Delayed(id2, f2)::ts2 -> async {
      let! o1 = f1 |> Async.AwaitFuture
      let! o2 = f2 |> Async.AwaitFuture
      return! unifyTypes assigns (o1::ts1) (o2::ts2) }      
      
  | t1::ts1, t2::ts2 when typesEqual t1 t2 -> unifyTypes assigns ts1 ts2

  | Type.Object(o1)::ts1, Type.Object(o2)::ts2 when arraysEqual o1.Members o2.Members membersEqual ->
      unifyTypes assigns (o1.Typeargs @ ts1) (o2.Typeargs @ ts2)
  | Type.Any::ts1, t::ts2 | t::ts1, Type.Any::ts2 -> unifyTypes assigns ts1 ts2
  | (Type.Parameter n)::ts1, t::ts2 | t::ts1, (Type.Parameter n)::ts2 -> 
      unifyTypes ((n, t)::assigns) ts1 ts2
  | Type.Function(tis1, to1)::ts1, Type.Function(tis2, to2)::ts2 ->
      unifyTypes assigns (to1::tis1 @ ts1) (to2::tis2 @ ts2)
  | Type.List(t1)::ts1, Type.List(t2)::ts2 -> 
      unifyTypes assigns (t1::ts1) (t2::ts2)
  | [], [] -> 
      async { return (fun res rng -> assigns, res) }
  | ts1, ts2 -> 
      async { return (fun res rng ->
          Log.error("typechecker", "Cannot unify types: %O and %O", Array.ofList ts1, Array.ofList ts2)
          assigns, addError (Errors.TypeChecker.cannotUnityTypes rng) res) }

let avoidCapture bound assigns =
  let renames = bound |> List.choose (fun n -> 
    let assigned = Map.tryFindKey (fun k v -> match v with Type.Parameter nn -> n = nn | _ -> false) assigns
    if assigned.IsSome then Some(n, n + "0") else None) 
  renames |> List.fold (fun assigns (o, n) -> Map.add o (Type.Parameter n) assigns) assigns,
  Map.ofSeq renames   

let rec substituteMembers assigns members = 
  members |> Array.map (function
    | Member.Method(n,tp,ars,t,d,e) -> 
        let assigns0 = assigns
        let assigns, renames = avoidCapture tp assigns
        // Log.trace("typechecker", "Substituting in %s<%s>, renaming: %O, assings: %O", n, String.concat "," tp, Map.toArray renames, Map.toArray assigns0)
        let tp = tp |> List.map (fun tp -> match Map.tryFind tp renames with Some r -> r | _ -> tp)
        let nars = ars |> List.map (fun (n,o,t) -> n, o, substituteTypes assigns t)
        Member.Method(n,tp,nars,substituteTypes assigns t,d,e)
    | Member.Property(n,t,s,d,e) -> Member.Property(n,substituteTypes assigns t,s,d,e))      

and substituteTypes assigns t =
  match t with
  | Type.Parameter s when Map.containsKey s assigns -> assigns.[s]
  | Type.Parameter _ | Type.Any | Type.Primitive _ | Type.Unit -> t
  | Type.Function(ts, t) -> Type.Function(List.map (substituteTypes assigns) ts, substituteTypes assigns t)
  | Type.List(t) -> Type.List(substituteTypes assigns t)
  | Type.Object(o) ->
      let members = substituteMembers assigns o.Members
      let bound = o.Typeargs |> List.choose (function Type.Parameter n -> Some n | _ -> None)
      let assigns, _ = avoidCapture bound assigns
      { Typeargs = List.map (substituteTypes assigns) o.Typeargs
        Members = members } |> Type.Object
  | Type.Delayed(g, f) -> 
      let f = 
        { new Future<Type> with 
            member x.Then(g) =             
              f.Then(fun t -> g (substituteTypes assigns t)) }
      Type.Delayed(g, f)

let rec applyTypes assigns t = 
  match t with 
  | Type.Delayed(g, f) ->
      let f = 
        { new Future<Type> with 
            member x.Then(g) =             
              f.Then(fun t -> g (applyTypes assigns t)) }
      Type.Delayed(g, f)
  | Type.Object(o) ->
      { Typeargs = List.map (substituteTypes assigns) o.Typeargs
        Members = substituteMembers assigns o.Members } |> Type.Object      
  | t when List.isEmpty (Map.toList assigns) -> t
  | Type.Any -> Type.Any
  | _ -> 
      Log.error("typechecker", "Invalid type application %O with %O", t, Map.toArray assigns)
      failwith "Invalid type application"

let rec typeCheckExpr ctx ctxTyp res (expr:Expr<unit>) = async {
  match expr.Expr with
  | ExprKind.Null ->
      return failwith "Unexpected null in source code."
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
  | ExprKind.String s -> 
      return { Expr = ExprKind.String s; Type = Type.Primitive "string"; Range = expr.Range }, res
  | ExprKind.Boolean b -> 
      return { Expr = ExprKind.Boolean b; Type = Type.Primitive "bool"; Range = expr.Range }, res

  | ExprKind.Function(n, e) ->
      let varTy = ctxTyp |> Option.bind (function
        | Type.Function([ty], _) -> Some ty | _ -> None)
      let varTy = defaultArg varTy Type.Any
      Log.trace("typechecker", "function: '%s -> ...' in context: %O", n.Name, varTy)
      let! e, res = typeCheckExpr (addVariable n.Name varTy ctx) None res e
      return { Expr = ExprKind.Function(n, e); Type = Type.Function([varTy], e.Type); Range = expr.Range }, res

  | ExprKind.List(els) ->
      let! res, els =  els |> Async.foldMap (typeCheckExpr ctx None) res
      let mutable tys = []
      for el in els do
        let known = tys |> List.exists (typesEqual el.Type)
        if not known then tys <- el.Type::tys
      let ty, res =
        match tys with
        | [] -> Type.Any, res // TODO: Something clever
        | [ty] -> ty, res
        | ty::_ -> Type.Any, res 
            // TODO: Ideally we would avoid explicit boxing
            // |> addError (Errors.TypeChecker.mismatchingListTypes expr.Range)
      return { Expr = ExprKind.List els; Type = Type.List(ty); Range = expr.Range }, res

  | ExprKind.Property(e, name) ->
      Log.trace("typechecker", "checking access %s", name.Name)
      let! typed, res = typeCheckExpr ctx None res e
      let! members = getObjectMembers typed.Type 
      Log.trace("typechecker", "checked access %s", name.Name)
      let resTyp, res = 
        match members with
        | ObjectMembers.Members members ->
            match members |> Seq.tryPick (function Member.Property(name=n; typ=r) when n = name.Name -> Some r | _ -> None) with
            | Some resTyp -> resTyp, res
            | _ -> Type.Any, res |> addError (Errors.TypeChecker.propertyMissing name.Range name.Name members)
        | ObjectMembers.SilentError -> Type.Any, res
        | ObjectMembers.NotAnObject -> Type.Any, res |> addError (Errors.TypeChecker.notAnObject e.Range typed.Type)
      return { Expr = ExprKind.Property(typed, name); Type = resTyp; Range = expr.Range }, res

  | ExprKind.Call(e, name, args) ->
      let! typed, res = typeCheckExpr ctx None res e
      let! members = getObjectMembers typed.Type 

      let positionBased, nameBased, res = 
        let pb = args |> List.takeWhile (fun arg -> arg.Name.IsNone) 
        let nb = args |> List.skipWhile (fun arg -> arg.Name.IsNone)
        pb |> List.map (fun arg -> arg.Value) |> Array.ofList,
        nb |> List.choose (fun arg -> arg.Name |> Option.map (fun n -> n.Name, arg.Value)) |> Map.ofList,
        match nb |> List.tryFind (fun arg -> arg.Name.IsNone) with
        | Some arg -> res |> addError (Errors.TypeChecker.nameBasedParamMustBeLast arg.Value.Range)
        | _ -> res
                
      let noRange = { Start = 0; End = 0 }
      let checkArguments pars = 
        pars |> List.mapi (fun i p -> i, p) |> Async.foldMap (fun res (index, (name, optional, typ)) -> async {
          let arg = 
            if index < positionBased.Length then Some(positionBased.[index]) 
            else Map.tryFind name nameBased 
          match arg with
          | Some arg -> 
              let! t, res = typeCheckExpr ctx (Some typ) res arg
              return { Name = Some { Name = name; Range = arg.Range }; Value = t }, res  // Here we are returning wrong range 
          | None when optional ->
              let v = { Expr = ExprKind.Null; Range = noRange; Type = typ }
              return { Name = Some { Name = name; Range = noRange }; Value = v }, res // dtto
          | _ ->
              let v = { Expr = ExprKind.Empty; Range = noRange; Type = typ }
              return { Name = Some { Name = name; Range = noRange }; Value = v }, res }) res
        

        //List.zip args argTys |> Async.foldMap (fun res (arg, argTy) -> async {
          //let! t, res = typeCheckExpr ctx argTy res arg.Value
          //return { Name = arg.Name; Value = t }, res }) res

      let! resTyp, typedArgs, res = async {
        match members with
        | ObjectMembers.Members members -> 
            match members |> Seq.tryPick (function Member.Method(name=n; typars=tp; arguments=args; typ=r) when n = name.Name -> Some(tp, r, args) | _ -> None) with
            | Some(tp, resTyp, pars) -> 
                // TODO: check arguments
                let! res, typedArgs = checkArguments pars
                let typedArgs = 
                  List.zip pars typedArgs |> List.map (fun ((n, _, _), ta) ->
                     { ta with Name = Some { Name = n; Range = ta.Value.Range }})

                let parTys = pars |> List.map (fun (_, _, t) -> t)
                let argTys = typedArgs |> List.map (fun a -> a.Value.Type)
                Log.trace("typechecker", "unifying %s: pars: %O, args: %O", name.Name, Array.ofList parTys, Array.ofList argTys)
                let! unifyFunc = unifyTypes [] parTys argTys 
                let assigns, res = unifyFunc res name.Range

                Log.trace("typechecker", "call %s: tyargs: %s, assigns: %O", name.Name, String.concat "," tp, Array.ofList assigns)
                let resTyp = applyTypes (Map.ofList assigns) resTyp
                Log.trace("typechecker", "call %s: result type: %O", name.Name, resTyp)
                return resTyp, typedArgs, res 
            | _ -> 
                //let! res, _ = checkArguments None
                let res = res |> addError (Errors.TypeChecker.methodMissing name.Range name.Name members)
                return Type.Any, [], res
        | ObjectMembers.SilentError -> 
            //let! res, typedArgs = checkArguments None
            return Type.Any, [], res
        | ObjectMembers.NotAnObject -> 
            //let! res, typedArgs = checkArguments None
            return Type.Any, [], res |> addError (Errors.TypeChecker.notAnObject e.Range typed.Type) }

      return { Expr = ExprKind.Call(typed, name, typedArgs); Type = resTyp; Range = expr.Range }, res }

let rec typeCheckCmd ctx res cmds = async {
  match cmds with
  | ({ Command = CommandKind.Let(name, expr) } as cmd)::cmds ->
      let! typed, res = typeCheckExpr ctx None res expr
      let! cmds, res = typeCheckCmd (addVariable name.Name typed.Type ctx) res cmds
      return { Command = CommandKind.Let(name, typed); Range = cmd.Range}::cmds, res
  | ({ Command = CommandKind.Expr(expr) } as cmd)::cmds ->      
      let! typed, res = typeCheckExpr ctx None res expr
      let! cmds, res = typeCheckCmd ctx res cmds
      return { Command = CommandKind.Expr(typed); Range = cmd.Range}::cmds, res
  | [] -> 
      return [], res }

let typeCheckProgram ctx res prog = async {
  let! cmds, res = typeCheckCmd ctx res prog.Body
  return { Body = cmds; Range = prog.Range }, res }


// ------------------------------------------------------------------------------------------------
// User friendly entry point
// ------------------------------------------------------------------------------------------------

open TheGamma.AstOperations

let needsEscaping (s:string) = 
  (s.[0] >= '0' && s.[0] <= '9') ||
  (s.ToCharArray() |> Array.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) ))

let escapeIdent s = 
  if needsEscaping s then "'" + s + "'" else s

let mapNameRanges f (n:Name) = 
  { n  with Range = f n.Range }

let rec mapExprRanges f expr = 
  match expr.Expr with  
  | ExprLeaf -> { expr with Range = f expr.Range }
  | ExprNode(es, ns) -> 
      { Expr = rebuildExprNode expr.Expr (List.map (mapExprRanges f) es) (List.map (mapNameRanges f) ns)
        Range = f expr.Range; Type = expr.Type }

let rec mapCmdRanges f cmd = 
  match cmd.Command with
  | CommandKind.Expr e -> { Command = CommandKind.Expr (mapExprRanges f e); Range = cmd.Range }
  | CommandKind.Let(n, e) -> { Command = CommandKind.Let(mapNameRanges f n, mapExprRanges f e); Range = cmd.Range }

let tokenize (input:string) = 
  let input = input.Replace("\r\n", "\n")
  let tokens, errors = Tokenizer.tokenize input
  List.ofArray errors, tokens

let parse (input:string) = 
  let errs1, tokens = tokenize input
  let (Parsec.Parser p) = Parser.program

  let rangeLookup = tokens |> List.map (fun tok -> tok.Range) |> Array.ofSeq

  let tokToChar rng =
    let safe start n = 
      if n >= rangeLookup.Length then rangeLookup.[rangeLookup.Length-1].End
      elif n < 0 then 0
      elif start then rangeLookup.[n].Start
      else rangeLookup.[n].End
    let rng = 
      { Start = safe true rng.Start
        End = safe false (rng.End-1) }
    if rng.End < rng.Start then { rng with End = rng.Start }
    else rng

  match p (0, tokens) with
  | Some((offs, rest), errs2, prog) ->
      let errs2 = errs2 |> List.map (fun e -> { e with Range = tokToChar e.Range })
      let errors = 
        if List.isEmpty rest then errs1 @ errs2
        else
          { Number = 21; Range = tokToChar { Start = offs; End = offs + List.length rest }
            Message = sprintf "Parser stopped: %A" rest } :: errs1 @ errs2
      errors, { Range = prog.Range; Body = prog.Body |> List.map (mapCmdRanges tokToChar) }
  | _ ->
    { Number = 21; Range = tokToChar { Start = 0; End = List.length tokens }
      Message = sprintf "Parser stopped: %A" tokens } :: errs1,
    { Range = tokToChar { Start = 0; End = List.length tokens }
      Body = [] }
          
let typeCheck globals input = async {
  let errs1, untyped = parse input
  let! checkd, ctx = typeCheckProgram { Variables = globals } { Errors = [] } untyped
  return errs1 @ ctx.Errors, checkd }


// ------------------------------------------------------------------------------------------------
// Collecting editors
// ------------------------------------------------------------------------------------------------

type EditorInfo = 
  { Source : string
    Completions : list<Range * Range * Member[]> }

let extendUntilDot (text:string) (rng:Range) = 
  let mutable start = rng.Start
  while start > 0 && text.[start] <> '.'  do start <- start - 1
  { rng with Start = start + 1 }

let withCompletion r t ctx = async {
  let! members = getObjectMembers t
  match members with
  | Members members -> return { ctx with Completions = (extendUntilDot ctx.Source r, r, members)::ctx.Completions }
  | _ -> return ctx }

let rec collectExprInfo ctx expr = async {
  match expr.Expr with
  | ExprKind.Property(inst, n) -> 
      let! ctx = collectExprInfo ctx inst
      return! withCompletion n.Range inst.Type ctx
  | ExprKind.Call(inst, n, args) ->
      let! ctx = collectExprInfo ctx inst
      let! ctx = args |> Async.fold (fun ctx arg -> collectExprInfo ctx arg.Value) ctx
      return! withCompletion n.Range inst.Type ctx 
  | ExprKind.List(els) ->
      return! Async.fold collectExprInfo ctx els
  | ExprKind.Function(_, e) ->
      return! collectExprInfo ctx e
  | ExprKind.Empty
  | ExprKind.Unit
  | ExprKind.Null
  | ExprKind.Number _
  | ExprKind.String _
  | ExprKind.Boolean _
  | ExprKind.Variable _ -> return ctx }

let rec collectCmdInfo ctx cmds = async {
  match cmds with
  | [] -> return ctx
  | { Command = CommandKind.Let(_, expr) }::cmds 
  | { Command = CommandKind.Expr(expr) }::cmds ->
      let! ctx = collectExprInfo ctx expr
      return! collectCmdInfo ctx cmds }

let collectProgramInfo ctx prog = 
  collectCmdInfo ctx prog.Body