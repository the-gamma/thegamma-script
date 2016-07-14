module TheGamma.CodeGenerator

open TheGamma
open TheGamma.Babel
open Fable.Extensions

type CompilationContext =
  { LineLengths : int list
    Globals : Map<string, Expression> }

let rangeToLoc ctx rng = 
  let rec asLoc lines offs lengths =
    match lengths with
    | l::lengths when offs <= l -> { line = lines; column = offs }
    | l::lengths -> asLoc (lines+1) (offs-l-1) lengths
    | [] -> { line = lines; column = offs  } // error? out of range
  Some { start = asLoc 1 rng.Start ctx.LineLengths 
         ``end`` = asLoc 1 rng.Start ctx.LineLengths }

let rec getEmitter name typ = async {
  match typ with
  | Type.Object(o) -> 
      return o.Members |> Seq.pick (function 
        Member.Method(name=n; emitter=e) | Member.Property(name=n; emitter=e) when n=name -> Some e | _ -> None) 
  | Type.Delayed(f) ->
      let! typ = Async.AwaitFuture f
      return! getEmitter name typ 
  | _ -> return failwith "Not an object" }

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
  | ExprKind.Number(n) ->
      return NumericLiteral(n, rangeToLoc ctx expr.Range)
  | ExprKind.Variable(n) when ctx.Globals.ContainsKey(n.Name) ->
      return ctx.Globals.[n.Name]
  | ExprKind.Variable(n) ->
      return IdentifierExpression(n.Name, rangeToLoc ctx n.Range) 
  | e ->
      Fable.Import.Browser.console.log("compileExpression: %O", e) 
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
