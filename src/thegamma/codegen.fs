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

let rec getEmitter name typ = async {
  match typ with
  | Type.Object(o) -> 
      return o.Members |> Seq.pick (function 
        Member.Method(name=n; emitter=e) | Member.Property(name=n; emitter=e) when n=name -> Some e | _ -> None) 
  | Type.Delayed(_, f) ->
      let! typ = Async.AwaitFuture f
      return! getEmitter name typ 
  | _ -> return failwith "getEmitter: Not an object" }

let rec compileExpression ctx (expr:Node<Expr>) = async {
  match expr.Node with 
  | Expr.Binary _ ->
      return failwith "compileExpression: Binary"
  | Expr.Call(inst, n, args) ->
      return failwith "compileExpression: Call"
      (*
      let! emitter = getEmitter n.Name inst.Type
      let! inst = compileExpression ctx inst
      let! args = args.Node |> Array.ofList |> Async.map (fun a -> async {
        let! r = compileExpression ctx a.Value
        return (match a.Name with Some n -> n.Name | _ -> ""), r })          // TODO: Names ...???
      return emitter.Emit(inst, args)
      *)
  | Expr.Property(inst, n) ->
      return failwith "compileExpression: Property"
      (*
      let! emitter = getEmitter n.Name inst.Type
      let! inst = compileExpression ctx inst
      return emitter.Emit(inst, [])
      *)
  //| Expr.Null ->
    //  return NullLiteral(rangeToLoc ctx expr.Range)
  | Expr.Number(n) ->
      return NumericLiteral(n, rangeToLoc ctx expr.Range)
  | Expr.String(s) ->
      return StringLiteral(s, rangeToLoc ctx expr.Range)
  | Expr.Boolean(b) ->
      return BooleanLiteral(b, rangeToLoc ctx expr.Range)
  | Expr.Variable(n) when ctx.Globals.ContainsKey(n.Node.Name) ->
      return ctx.Globals.[n.Node.Name]
  | Expr.Variable(n) ->
      return IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range) 
  | Expr.List(es) ->
      let! es = Async.map (compileExpression ctx) es
      return ArrayExpression(es, rangeToLoc ctx expr.Range)
  | Expr.Function(n, e) ->
      let var = IdentifierExpression(n.Node.Name, rangeToLoc ctx n.Range)
      let! ce = compileExpression { ctx with Globals = Map.add n.Node.Name var ctx.Globals } e
      let body = BlockStatement([ReturnStatement(ce, rangeToLoc ctx e.Range)], rangeToLoc ctx e.Range)
      return FunctionExpression(None, [IdentifierPattern(n.Node.Name, rangeToLoc ctx n.Range)], body, false, false, rangeToLoc ctx expr.Range)
  //| Expr.Unit
  | Expr.Empty ->      
      Fable.Import.Browser.console.log("compileExpression: %O", expr.Node) 
      return failwith "!" }
    

let compileCommand ctx (cmd:Node<Command>) = async {
  match cmd.Node with
  | Command.Let(n, e) ->
      let! e = compileExpression ctx e
      let name = IdentifierPattern(n.Node.Name, rangeToLoc ctx n.Range)
      let decl = VariableDeclarator(name, Some e, rangeToLoc ctx cmd.Range)
      return VariableDeclaration(Var, [decl], rangeToLoc ctx cmd.Range)
  | Command.Expr(e) ->
      let! e = compileExpression ctx e
      return ExpressionStatement(e, rangeToLoc ctx cmd.Range) }

let compileProgram ctx (prog:TheGamma.Program) = async {
  let! body = Async.map (compileCommand ctx) prog.Body.Node
  return { location = rangeToLoc ctx prog.Body.Range; body = body } }

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

let compileAndRun globals (text:string) prog = async {
  try
    let! globals = Async.AwaitFuture globals
    let ctx = { LineLengths = [ for l in text.Split('\n') -> l.Length ]; Globals = globals }  
    let! res = compileProgram ctx prog
    let code = babel.transformFromAst(Serializer.serializeProgram res, text, { presets = [| "es2015" |] })
    Log.trace("codegen", "Evaluating: %O", code)
    return code.code;
  with e ->
    Log.exn("codegen", "Evaluating code failed: %O", e)
    return "" }
