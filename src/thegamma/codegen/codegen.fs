// ------------------------------------------------------------------------------------------------
// Code generator is used to compile complete well-typed programs
// ------------------------------------------------------------------------------------------------
module TheGamma.CodeGenerator

open TheGamma
open TheGamma.Babel
open TheGamma.Babel.BabelOperators
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// Compiling code to Babel AST
// ------------------------------------------------------------------------------------------------

type CompilationContext =
  { LineLengths : int list
    Globals : Map<string, Expression> }

let rec offsetToLocation lines offs lengths =
  match lengths with
  | l::lengths when offs <= l -> { line = lines; column = offs }
  | l::lengths -> offsetToLocation (lines+1) (offs-l-1) lengths
  | [] -> failwith "offsetToLocation: Out of range" // { line = lines; column = offs  } 

let rangeToLoc ctx rng = 
  { start = offsetToLocation 1 rng.Start ctx.LineLengths 
    ``end`` = offsetToLocation 1 rng.Start ctx.LineLengths } |> Some

let rec getMember name typ = 
  match typ with
  | Type.Object(o) -> 
      match o.Members |> Seq.tryPick (fun m -> if m.Name = name then Some(m) else None) with
      | Some res -> res
      | _ ->
        Log.exn("codegen", "getMember: Member %s not found in object %O", name, o)
        failwith "getMember: Member not found" 
  | t -> 
    Log.exn("codegen", "getMember: Not an object %O", t)
    failwith "getMember: Not an object" 

let rec compileExpression ctx (expr:Node<Expr>) = 
  Log.trace("codegen", "Compiling expression: %O", expr)
  match expr.Node with 
  // Binary operators map to BinaryExpression, except for pow, which is a JS function
  | Expr.Binary(l, { Node = Operator.Power }, r) ->
      let l = compileExpression ctx l
      let r = compileExpression ctx r
      let rng = rangeToLoc ctx expr.Range
      let pow = MemberExpression(IdentifierExpression("pow", rng), IdentifierExpression("Math", rng), false, rng)
      CallExpression(pow, [l; r], rangeToLoc ctx expr.Range)

  | Expr.Binary(l, op, r) ->
      let l = compileExpression ctx l
      let r = compileExpression ctx r
      let op = 
        match op.Node with
        | Operator.Equals -> BinaryEqualStrict
        | Operator.Plus -> BinaryPlus
        | Operator.Minus -> BinaryMinus
        | Operator.Multiply -> BinaryMultiply
        | Operator.Divide -> BinaryDivide
        | Operator.GreaterThan -> BinaryGreater
        | Operator.LessThan -> BinaryLess
        | Operator.GreaterThanOrEqual -> BinaryGreaterOrEqual
        | Operator.LessThanOrEqual -> BinaryLessOrEqual
        | Operator.Power -> failwith "compileExpression: Power is not a binary operator"
      BinaryExpression(op, l, r, rangeToLoc ctx expr.Range)
      
  // Handle member access and calls - method call is a combination of the two
  | Expr.Member(inst, { Node = Expr.Placeholder(_, { Node = Expr.Variable n }) })
  | Expr.Member(inst, { Node = Expr.Variable n }) ->
      let mem = getMember n.Node.Name inst.Entity.Value.Type.Value
      let inst = compileExpression ctx inst
      mem.Emitter.Emit(inst)

  | Expr.Member(inst, _) ->
      failwith "compileExpression: Member in member access is not a variable"

  | Expr.Call(inst, args) ->
      // Split arguments between position & name based
      let compiledArgs = args.Node |> List.map (fun a -> a.Name, compileExpression ctx a.Value)
      let positionArgs = compiledArgs |> Seq.takeWhile (fun (n, _) -> n.IsNone) |> Seq.map snd |> Array.ofSeq
      let namedArgs = compiledArgs |> Seq.choose (function (Some n, a) -> Some(n.Node.Name, a) | _ -> None) |> dict

      // Get expected arguments from the method type
      let expectedArgs = 
        match inst.Entity.Value.Type.Value with
        | Type.Method(args, resTy) -> args
        | _ -> []

      // Compile the instance, the arguments and call the emitter
      let inst = compileExpression ctx inst
      let pars = expectedArgs |> List.mapi (fun i (name, _, _) ->
        if i < positionArgs.Length then positionArgs.[i]
        elif namedArgs.ContainsKey name then namedArgs.[name]
        else NullLiteral(rangeToLoc ctx args.Range))
      CallExpression(inst, pars, rangeToLoc ctx expr.Range)

  // Variables and literals are easy       
  | Expr.Variable(n) when ctx.Globals.ContainsKey(n.Node.Name) ->
      ctx.Globals.[n.Node.Name]
  | Expr.Variable(n) ->
      IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range) 

  | Expr.Number(n) ->
      NumericLiteral(n, rangeToLoc ctx expr.Range)
  | Expr.String(s) ->
      StringLiteral(s, rangeToLoc ctx expr.Range)
  | Expr.Boolean(b) ->
      BooleanLiteral(b, rangeToLoc ctx expr.Range)

  // Other constructs that map fairly directly to JS 
  | Expr.Placeholder(_, body) ->
      compileExpression ctx body

  | Expr.List(es) ->
      let es = List.map (compileExpression ctx) es
      ArrayExpression(es, rangeToLoc ctx expr.Range)

  | Expr.Function(n, e) ->
      let var = IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range)
      let ce = compileExpression { ctx with Globals = Map.add n.Node.Name var ctx.Globals } e
      let body = BlockStatement([ReturnStatement(ce, rangeToLoc ctx e.Range)], rangeToLoc ctx e.Range)
      FunctionExpression(None, [IdentifierPattern(n.Node.Name, rangeToLoc ctx n.Range)], body, false, false, rangeToLoc ctx expr.Range)

  // Empty expressions should not happen...
  | Expr.Empty ->      
      Log.error("codegen", "getEmitterAndParams: Empty expression in the AST")
      NullLiteral(rangeToLoc ctx expr.Range)
    

let compileCommand ctx (cmd:Node<Command>) = 
  match cmd.Node with
  | Command.Let(n, e) ->
      let e = compileExpression ctx e
      let name = IdentifierPattern(n.Node.Name, rangeToLoc ctx n.Range)
      let decl = VariableDeclarator(name, Some e, rangeToLoc ctx cmd.Range)
      VariableDeclaration(Var, [decl], rangeToLoc ctx cmd.Range)
  | Command.Expr(e) ->
      let e = compileExpression ctx e
      ExpressionStatement(e, rangeToLoc ctx cmd.Range)


let compileProgram ctx (prog:TheGamma.Program) = 
  let body = List.map (compileCommand ctx) prog.Body.Node
  { location = rangeToLoc ctx prog.Body.Range; body = body }

// ------------------------------------------------------------------------------------------------
// Cmpile program and return JS source code
// ------------------------------------------------------------------------------------------------

open Fable.Helpers.Babel

let compile globals (text:string) prog = async {
  try
    let! globals = Async.AwaitFuture globals
    let globals = 
      globals |> List.choose (function
        | { Kind = EntityKind.GlobalValue(n, Some e) } -> Some(n.Name, e)
        | _ -> None ) |> Map.ofSeq    
    let ctx = { LineLengths = [ for l in text.Split('\n') -> l.Length ]; Globals = globals }  
    let res = compileProgram ctx prog
    let code = babel.transformFromAst(Serializer.serializeProgram res, text, { presets = [| "es2015" |] })
    Log.trace("codegen", "Evaluating: %O", code)
    return code.code

  with e ->
    Log.exn("codegen", "Evaluating code failed: %O", e)
    return "" }
