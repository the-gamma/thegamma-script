// ------------------------------------------------------------------------------------------------
// Interpreter is used to partially evaluate parts of program as needed
// ------------------------------------------------------------------------------------------------
module TheGamma.Interpreter

open TheGamma
open TheGamma.Ast
open TheGamma.Common
open TheGamma.Babel
open TheGamma.Babel.BabelOperators
open Fable.Helpers.Babel
open System.Collections.Generic

// ------------------------------------------------------------------------------------------------
// Wrappers around `eval` that let us treat runtime values as `Expressions` we can pass to babel
// ------------------------------------------------------------------------------------------------

/// Creates an array of objects and list of expressions that refer
/// to them as if they were stored in an array, e.g. `_stored[0]` and `_stored[1]`
let storeArguments values =
  values |> Array.ofList, 
  values |> List.mapi (fun i _ ->
    MemberExpression
      ( IdentifierExpression("_stored", None),
        NumericLiteral(float i, None), true, None ))

/// Evalaute Babel expression, assuming `_stored` is in scope
let evaluateExpression (_stored:RuntimeValue[]) (expr:Expression) =
  let prog = { Babel.Program.location = None; Babel.Program.body = [ExpressionStatement(expr, None)] }
  let code = babel.transformFromAst(Serializer.serializeProgram prog, "", { presets = [| "es2015" |] })
  Log.trace("interpreter", "Interpreter evaluating: %O using values %O", code.code, _stored)
  try
    // HACK (1/2): Get fable to reference everything
    let s = TheGamma.Series.series<int, int>.create(async { return [||] }, "", "", "") 
    TheGamma.TypeProvidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
    TheGamma.TypeProvidersRuntime.trimLeft |> ignore
    TheGamma.TypeProvidersRuntime.convertTupleSequence |> ignore
    TheGamma.GoogleCharts.chart.bar |> ignore
    TheGamma.table<int, int>.create(s) |> ignore
    TheGamma.Maps.timeline<int, int>.create(s) |> ignore
    TheGamma.Series.series<int, int>.values([| 1 |]) |> ignore    
    TheGamma.placeholder.create("") |> ignore
    TheGamma.Interactive.youdraw.create |> ignore

    // HACK (2/2) The name `_stored` may appear in the generated code!
    _stored.Length |> ignore
    eval(code.code)
  with e ->
    Log.exn("interpreter", "Evaluation failed: %O", e)
    reraise()

/// Store given arguments and evalaute expression
let evaluateExpr args exprBuilder =
  let _stored, args = storeArguments args
  evaluateExpression _stored (exprBuilder args)

/// If the value is object with 'preview' method or property, evaluate it!
let evaluatePreview (ent:Entity) value = 
  let previewName = {Name.Name="preview"}
  Log.trace("interpreter", "Evaluating preview on: %O (%s)", ent, Ast.formatType ent.Type.Value)
  match ent.Type with
  | Some(Type.Object(FindMember previewName mem)) ->       
      // Member access or member access & call, depending on whether the member is a method
      match mem.Type with
      | Type.Method(_, _) -> evaluateExpr [value] (fun inst -> mem.Emitter.Emit(List.head inst) /@/ []) |> Some
      | _ -> evaluateExpr [value] (fun inst -> mem.Emitter.Emit(List.head inst)) |> Some
  | _ -> None

// ------------------------------------------------------------------------------------------------
// Recursively walk over entities & evaluate (starting from antecedents)
// ------------------------------------------------------------------------------------------------

let rec evaluateEntity (e:Entity) = 
  match e.Kind with
  // Constants, variables & global values (using expression stored in GlobalValue entity)
  | EntityKind.Constant(Constant.Boolean b) -> Some(unbox b)
  | EntityKind.Constant(Constant.Number n) -> Some(unbox n)
  | EntityKind.Constant(Constant.String s) -> Some(unbox s)
  | EntityKind.Constant(Constant.Empty) -> Some(unbox null)

  | EntityKind.Variable(_, value) ->
      value.Value |> Option.map (fun v -> v.Value)

  | EntityKind.GlobalValue(name, expr) ->
      match expr with
      | Some expr -> Some(evaluateExpression [| |] expr)
      | _ -> None

  // Member access and call - method call is member access followed by a call
  | EntityKind.Member(inst, { Kind = EntityKind.MemberName(name) }) ->
      match inst.Type.Value with 
      | Type.Object(FindMember name mem) -> 
          Some(evaluateExpr [getValue inst] (fun inst -> mem.Emitter.Emit(List.head inst)))
      | _ -> None

  | EntityKind.Call(inst, { Kind = EntityKind.ArgumentList(args) }) ->
      // Split arguments between index-based and position-based
      let pb = args |> List.takeWhile (function { Kind = EntityKind.NamedParam _ } -> false | _ -> true)  
      let nb = args |> List.skipWhile (function { Kind = EntityKind.NamedParam _ } -> false | _ -> true)  

      let positionBased = 
        pb |> List.map (getValue) |> Array.ofList
      let nameBased =   
        nb |> List.choose(function 
          | { Kind = EntityKind.NamedParam(name, value) } -> Some(name.Name, getValue value)
          | _ -> None) |> dict

      // Get expected arguments from the method type
      let expectedArgs = 
        match inst.Type.Value with
        | Type.Method(args, resTy) -> args
        | _ -> []

      // Evalate arguments and instance and run the call 
      let pars = expectedArgs |> List.mapi (fun i (name, _, _) ->
        if i < positionBased.Length then positionBased.[i]
        elif nameBased.ContainsKey(name) then nameBased.[name]
        else (unbox null) )

      match inst with 
      | { Kind = EntityKind.Member(inst, { Kind = EntityKind.MemberName(n) }) } ->
          let inst = getValue inst
          evaluateExpr (inst::pars) (fun stored -> ((List.head stored) /?/ str n.Name) /@/ List.tail stored)
      | _ ->
          let inst = getValue inst
          evaluateExpr (inst::pars) (fun stored -> List.head stored /@/ List.tail stored)

  | EntityKind.Member(inst, _) ->
      Log.error("interpreter", "typeCheckEntity: Member access is missing member name!")
      None
  | EntityKind.Call(inst, _) ->
      Log.error("interpreter", "typeCheckEntity: Call to %s is missing argument list!", (lastChainElement inst).Name)
      None

  // Binary operators - most map to JavaScript except for power, which is a JS function
  | EntityKind.Operator(l, Operator.Power, r) ->
      evaluateExpr [getValue l; getValue r] (function 
        | [l; r] -> ident("Math")?pow /@/ [l; r]
        | _ -> failwith "evaluateEntity: Expected two arguments") |> Some      

  | EntityKind.Operator(l, op, r) ->
      evaluateExpr [getValue l; getValue r] (function 
        | [l; r] -> 
            let op = 
              match op with
              | Operator.Equals -> BinaryEqualStrict
              | Operator.Plus -> BinaryPlus
              | Operator.Minus -> BinaryMinus
              | Operator.Multiply -> BinaryMultiply
              | Operator.Divide -> BinaryDivide
              | Operator.GreaterThan -> BinaryGreater
              | Operator.LessThan -> BinaryLess
              | Operator.GreaterThanOrEqual -> BinaryGreaterOrEqual
              | Operator.LessThanOrEqual -> BinaryLessOrEqual
              | Operator.Power -> failwith "evaluateEntity: Power is not a binary operation"
            BinaryExpression(op, l, r, None)
        | _ -> failwith "evaluateEntity: Expected two arguments") |> Some            

  // Other simple language constructs
  | EntityKind.List(ents) ->
      evaluateExpr (List.map (getValue) ents) (fun elements ->
        ArrayExpression(elements, None)) |> Some

  | EntityKind.Placeholder(_, body) ->
      Some(getValue body)

  // The following entities do not represent anything that has a value      
  | EntityKind.ArgumentList _
  | EntityKind.NamedParam _
  | EntityKind.MemberName _
  | EntityKind.Binding _
  | EntityKind.Root _
  | EntityKind.CallSite _ ->
      Some(unbox null)

  | EntityKind.Function _
  | EntityKind.Program _ 
  | EntityKind.LetCommand _ 
  | EntityKind.RunCommand _ -> 
      Log.error("interpreter", "Cannot evaluate entity (probably not supported yet): %O", e)
      None

// Evaluate value and lazily generate preview, if it is None
and ensureValue (e:Entity) = 
  if e.Value.IsNone then
    match evaluateEntity e with
    | Some value ->
        e.Value <- Some { Value = value; Preview = Lazy.Create(fun () -> evaluatePreview e value) }
    | _ -> ()

/// Get value assumes that `evaluateEntityTree` evaluated antecedents already
and getValue (e:Entity) = 
  if e.Value.IsNone then Log.error("interpreter", "getValue: Value of entity %O has not been evaluated.", e)
  e.Value.Value.Value

/// Evalaute antecedents (caching them in `visited`) and then evalaute `e`
let evaluateEntityTree (e:Entity) = 
  let visited = Dictionary<Symbol, bool>()
  let rec loop (e:Entity) = 
    if not (visited.ContainsKey(e.Symbol)) && e.Value.IsNone then
      visited.[e.Symbol] <- true
      for e in e.Antecedents do loop e
      ensureValue e
  loop e
  e.Value 

// ------------------------------------------------------------------------------------------------
// Public interface - creating global entities and evaluating entities
// ------------------------------------------------------------------------------------------------

let globalEntity name meta typ expr = 
  { Kind = EntityKind.GlobalValue({ Name = name }, expr)
    Symbol = createSymbol()
    Type = Some typ
    Meta = meta
    Value = None
    Errors = [] }

let evaluate (globals:seq<Entity>) (e:Entity) = 
  //Log.trace("interpreter", "Evaluating entity %s (%O)", e.Name, e.Kind)
  let res = evaluateEntityTree e
  //Log.trace("interpreter", "Evaluated entity %s (%O) = %O", e.Name, e.Kind, res)
  res
