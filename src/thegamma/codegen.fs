module TheGamma.CodeGenerator

open TheGamma
open TheGamma.Babel
open Fable.Extensions

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

let rec compileExpression ctx (expr:Expr<Type>) = async {
  match expr.Expr with 
  | ExprKind.Call(inst, n, args) ->
      let! emitter = getEmitter n.Name inst.Type
      let! inst = compileExpression ctx inst
      let! args = args |> Async.map (fun a -> async {
        let! r = compileExpression ctx a.Value
        return (match a.Name with Some n -> n.Name | _ -> ""), r })          // TODO: Names ...???
      return emitter.Emit(inst, args)
  | ExprKind.Property(inst, n) ->
      let! emitter = getEmitter n.Name inst.Type
      let! inst = compileExpression ctx inst
      return emitter.Emit(inst, [])
  | ExprKind.Null ->
      return NullLiteral(rangeToLoc ctx expr.Range)
  | ExprKind.Number(n) ->
      return NumericLiteral(n, rangeToLoc ctx expr.Range)
  | ExprKind.String(s) ->
      return StringLiteral(s, rangeToLoc ctx expr.Range)
  | ExprKind.Boolean(b) ->
      return BooleanLiteral(b, rangeToLoc ctx expr.Range)
  | ExprKind.Variable(n) when ctx.Globals.ContainsKey(n.Name) ->
      return ctx.Globals.[n.Name]
  | ExprKind.Variable(n) ->
      return IdentifierExpression(n.Name, rangeToLoc ctx n.Range) 
  | ExprKind.List(es) ->
      let! es = Async.map (compileExpression ctx) es
      return ArrayExpression(es, rangeToLoc ctx expr.Range)
  | ExprKind.Function(n, e) ->
      let var = IdentifierExpression(n.Name, rangeToLoc ctx n.Range)
      let! ce = compileExpression { ctx with Globals = Map.add n.Name var ctx.Globals } e
      let body = BlockStatement([ReturnStatement(ce, rangeToLoc ctx e.Range)], rangeToLoc ctx e.Range)
      return FunctionExpression(None, [IdentifierPattern(n.Name, rangeToLoc ctx n.Range)], body, false, false, rangeToLoc ctx expr.Range)
  | ExprKind.Unit
  | ExprKind.Empty ->      
      Fable.Import.Browser.console.log("compileExpression: %O", expr.Expr) 
      return failwith "!" }
    

let compileCommand ctx (cmd:Command<Type>) = async {
  match cmd.Command with
  | CommandKind.Let(n, e) ->
      let! e = compileExpression ctx e
      let name = IdentifierPattern(n.Name, rangeToLoc ctx n.Range)
      let decl = VariableDeclarator(name, Some e, rangeToLoc ctx cmd.Range)
      return VariableDeclaration(Var, [decl], rangeToLoc ctx cmd.Range)
  | CommandKind.Expr(e) ->
      let! e = compileExpression ctx e
      return ExpressionStatement(e, rangeToLoc ctx cmd.Range) }

let compileProgram ctx (prog:Program<Type>) = async {
  let! body = Async.map (compileCommand ctx) prog.Body
  return { location = rangeToLoc ctx prog.Range; body = body } }

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
