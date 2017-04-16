module TheGamma.CodeGenerator

open TheGamma
open TheGamma.Babel
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
  | [] -> { line = lines; column = offs  } // error? out of range

let rangeToLoc ctx rng = 
  Some { start = offsetToLocation 1 rng.Start ctx.LineLengths 
         ``end`` = offsetToLocation 1 rng.Start ctx.LineLengths }

let rec getEmitterAndParams name typ = 
  match typ with
  | Type.Object(o) -> 
      o.Members |> Seq.pick (function 
        | Member.Method(name=n; arguments=args; emitter=e) when n = name -> Some(e, args)
        | Member.Property(name=n; emitter=e) when n=name -> Some(e, []) 
        | _ -> None) 
  | t -> 
    Log.exn("codegen", "getEmitterAndParams: Not an object %O", t)
    failwith "getEmitterAndParams: Not an object" 

let rec compileExpression ctx (expr:Node<Expr>) = 
  match expr.Node with 
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
        | Operator.Power -> failwith "compileExpression: Power is not a binary operation"
      BinaryExpression(op, l, r, rangeToLoc ctx expr.Range)
      
  | Expr.Call(Some inst, n, args) ->
      // Split arguments between position & name based
      let compiledArgs = args.Node |> List.map (fun a -> a.Name, compileExpression ctx a.Value)
      let positionArgs = compiledArgs |> Seq.takeWhile (fun (n, _) -> n.IsNone) |> Seq.map snd |> Array.ofSeq
      let namedArgs = compiledArgs |> Seq.choose (function (Some n, a) -> Some(n.Node.Name, a) | _ -> None) |> dict

      // Compile the instance
      let emitter, pars = getEmitterAndParams n.Node.Name inst.Entity.Value.Type.Value
      let inst = compileExpression ctx inst
      let pars = pars |> List.mapi (fun i (name, _, _) ->
        if i < positionArgs.Length then positionArgs.[i]
        elif namedArgs.ContainsKey name then namedArgs.[name]
        else NullLiteral(rangeToLoc ctx args.Range))
      emitter.Emit(inst, pars)

  | Expr.Call(None, n, args) ->
      failwith "compileExpression: Call without instance is not supported"

  | Expr.Property(inst, n) ->
      let emitter, _ = getEmitterAndParams n.Node.Name inst.Entity.Value.Type.Value
      let inst = compileExpression ctx inst
      emitter.Emit(inst, [])
      
  //| Expr.Null ->
    //  NullLiteral(rangeToLoc ctx expr.Range)
  | Expr.Number(n) ->
      NumericLiteral(n, rangeToLoc ctx expr.Range)
  | Expr.String(s) ->
      StringLiteral(s, rangeToLoc ctx expr.Range)
  | Expr.Boolean(b) ->
      BooleanLiteral(b, rangeToLoc ctx expr.Range)
  | Expr.Variable(n) when ctx.Globals.ContainsKey(n.Node.Name) ->
      ctx.Globals.[n.Node.Name]
  | Expr.Variable(n) ->
      IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range) 
  | Expr.List(es) ->
      let es = List.map (compileExpression ctx) es
      ArrayExpression(es, rangeToLoc ctx expr.Range)
  | Expr.Function(n, e) ->
      let var = IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range)
      let ce = compileExpression { ctx with Globals = Map.add n.Node.Name var ctx.Globals } e
      let body = BlockStatement([ReturnStatement(ce, rangeToLoc ctx e.Range)], rangeToLoc ctx e.Range)
      FunctionExpression(None, [IdentifierPattern(n.Node.Name, rangeToLoc ctx n.Range)], body, false, false, rangeToLoc ctx expr.Range)
  | Expr.Empty ->      
      Fable.Import.Browser.console.log("compileExpression: %O", expr.Node) 
      failwith "!" 
    

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
// Running compiled ASTs
// ------------------------------------------------------------------------------------------------

type BabelOptions = 
  { presets : string[] }

type BabelResult = 
  { code : string }
type Babel =
  abstract transformFromAst : obj * string * BabelOptions -> BabelResult

[<Emit("Babel")>]
let babel : Babel = Unchecked.defaultof<_> 

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
    return code.code;
  with e ->
    Log.exn("codegen", "Evaluating code failed: %O", e)
    return "" }
