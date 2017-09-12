module TheGamma.Main

open Fable.Core
open Fable.Core.Extensions
open Fable.Import
open Fable.Helpers
open Fable.Import.Browser

open TheGamma
open TheGamma.Ast
open TheGamma.Html
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.Services
open TheGamma.TypeProviders 
open TheGamma.Live.Common

module FsOption = Microsoft.FSharp.Core.Option

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

let buildGlobalsTable provideTypes = Async.StartAsNamedFuture "buildGlobalsTable" <| async {
  // We need to pass the lookup function to the code that provides types
  // (because the providers may need to lookup named types), so we define
  // the map as mutable and fill it later.
  let mutable named = Map.empty
  let lookupNamed n = 
    match named.TryFind(n) with
    | Some(r) -> r
    | None -> 
        Log.error("typechecker", "Could not find named type '%s'", n)
        failwith (sprintf "Could not find named type '%s'" n)

  let! provided = provideTypes lookupNamed
  let allTypes = 
    [ // Pretend we support these - the names appear in the F# provided types
      // and if the functions are not actually used, providing Any type works 
      yield TypeProviders.NamedType("value", Type.Any)
      yield TypeProviders.NamedType("object", Type.Any)
      yield TypeProviders.NamedType("seq", Type.Any) 
      yield TypeProviders.NamedType("async", Type.Any) 
      yield! provided ]

  // Build lookup table from named types and
  // list of global entities (provided global values)
  named <- 
    allTypes
    |> Seq.choose (function TypeProviders.NamedType(s, t) -> Some(s, t) | _ -> None)
    |> Map.ofSeq
  let globalEntities = allTypes |> List.choose (function 
    | TypeProviders.GlobalValue(n, m, e, t) -> 
        Some(Interpreter.globalEntity n m t (Some e))
    | _ -> None)
    (*
  let test = Interpreter.globalEntity "magic" [] (Type.Method(fun vs ->
    { new ObjectType with
        member x.Members = 
          [| for i in 1 .. unbox (List.item 1 vs) -> 
               { Name = unbox (List.head vs) + " " + string i; Type = Type.Primitive(PrimitiveType.String);
                 Metadata = []; Emitter = { Emit = fun _ -> Babel.StringLiteral(unbox (List.head vs), None) } } |]
        member x.TypeEquals _ = false } |> Type.Object |> Some )) (Some(Babel.StringLiteral("test", None)))
        *)
  return globalEntities } 

let rec resolveProvider lookup ignoreFilter kind endpoint = 
  match kind with
  | "rest" ->
      match TypeProviders.RestProvider.provideRestType lookup (resolveProvider lookup ignoreFilter) "anonymous" endpoint "" with
      | ProvidedType.GlobalValue(_, _, e, t) -> t, { Emit = fun _ -> e }
      | _ -> failwith "resolveProvider: Expected global value"
  | "pivot" ->
      let pivotType = async {
        let! typ = TypeProviders.Pivot.providePivotType endpoint ignoreFilter "anonymous" lookup
        match typ with 
        | ProvidedType.GlobalValue(_, _, _, t) -> return t 
        | _ -> return failwith "resolveProvider: Expected global value" }
      Type.Delayed(Async.StartAsNamedFuture ("pivotType:" + endpoint) pivotType),
      { Emit = fun _ -> TypeProviders.Pivot.makePivotExpression endpoint }
  | _ ->
    Log.error("providers", "Cannot resolve provider '%s' (%s)", kind, endpoint) 
    failwith "resolveProvider: Cannot resolve type provider"

// ------------------------------------------------------------------------------------------------
// JavaScript API
// ------------------------------------------------------------------------------------------------

type TheGammaProviders = 
  { globals : Future<Entity list> }

type TheGammaContext =
  { checkingService : CheckingService 
    providers : TheGammaProviders }

let callShowMethod outputId (cmd:Node<_>) = 
  match cmd.Node with
  | Command.Expr({ Entity = Some { Type = Some typ } } as inst) ->
      match typ with
      | Type.Object obj ->
          let showTyp = obj.Members |> Array.tryPick (function 
            | { Name = "show"; Type = typ & Type.Method([{ Type = Type.Primitive PrimitiveType.String }], _) } -> Some typ
            | _ -> None)
          match showTyp with 
          | Some showTyp ->
            let rng = { Range.Start = cmd.Range.End; End = cmd.Range.End }
            let outExpr = Ast.node rng (Expr.String(outputId))
            let args = [{ Argument.Name = None; Argument.Value = outExpr }]
            let showMember = Expr.Member(inst, Ast.node rng (Expr.Variable(Ast.node rng { Name = "show" }))) |> Ast.node rng
            let showEntity = 
              { Kind = EntityKind.Root; Symbol = createSymbol(); Value = None
                Meta = []; Type = Some showTyp; Errors = [] }
            showMember.Entity <- Some showEntity
            let expr = Ast.node rng (Expr.Call(showMember, Ast.node rng args))
            Ast.node cmd.Range (Command.Expr(expr))
          | _ -> cmd
      | _ -> cmd
  | _ -> cmd

[<Emit("eval($0)")>]
let eval (s:string) : obj = failwith "eval"

let evaluate ctx code outputId = async {
  // Type check & insert 'show' calls if 'outputId' is given
  let! _, _, prog = ctx.checkingService.TypeCheck(code)
  let newBody = 
    match outputId with
    | Some outputId -> prog.Body.Node |> List.map (callShowMethod outputId)
    | _ -> prog.Body.Node 
  let prog = { prog with Body = { prog.Body with Node = newBody } }
  let! code = CodeGenerator.compile ctx.providers.globals code prog 

  // Get fable to reference everything
  let s = TheGamma.Series.series<int, int>.create(async { return [||] }, "", "", "") 
  TheGamma.TypeProvidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
  TypeProvidersRuntime.trimLeft |> ignore
  TheGamma.GoogleCharts.chart.bar |> ignore
  TheGamma.table<int, int>.create(s) |> ignore
  TheGamma.General.date.now() |> ignore
  TheGamma.Series.series<int, int>.values([| 1 |]) |> ignore
  TheGamma.placeholder.create("") |> ignore
  TheGamma.Interactive.youguess.line |> ignore
  TheGamma.General.test("") |> ignore

  Log.trace("interpreter", "Main evaluating: %O", code)
  return eval code }

type provider = string -> (string -> Type) -> Async<list<ProvidedType>>

let previews = 
  [ Live.Pivot.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> 
    Live.Showable.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> ]

type editorOptions =
  { width : float option
    height : float option
    maxHeight : float option
    autoHeight : bool option
    enablePreview : bool option
    monacoOptions : (monaco.editor.IEditorConstructionOptions -> unit) option }

type error = 
  { number : int
    message : string
    startLine : int
    startColumn : int
    endLine : int
    endColumn : int }

let defaultEditorOptions = 
  { width = None; height = None; maxHeight = None; autoHeight = None; monacoOptions = None; enablePreview = None }

type thenable<'R>(work:Async<'R>) = 
  let mutable resCell = Choice1Of3()
  let mutable trigger = id
  do async { 
    try 
      let! res = work
      resCell <- Choice2Of3 res
      trigger ()
    with e ->
      resCell <- Choice3Of3 e 
      trigger () } |> Async.StartImmediate
  member x.``then``(onValue, ?onError) = 
    trigger <- fun () ->
      match resCell with 
      | Choice1Of3 () -> ()
      | Choice2Of3 res -> trigger <- id; onValue res
      | Choice3Of3 err -> trigger <- id; if onError.IsSome then onError.Value err
    trigger ()

let rec serializeType typ = 
  match typ with
  | Type.Any -> box "any"
  | Type.Delayed(_) -> box "delayed"
  | Type.Primitive(PrimitiveType.Unit) -> box "unit"
  | Type.Primitive(PrimitiveType.Bool) -> box "bool"
  | Type.Primitive(PrimitiveType.Date) -> box "date"
  | Type.Primitive(PrimitiveType.Number) -> box "number"
  | Type.Primitive(PrimitiveType.String) -> box "string"
  | Type.Method(args, res) -> 
      [ "kind", box "function"
        "arguments", args |> List.map (fun ma -> [| box ma.Name; box ma.Optional; box ma.Static; serializeType ma.Type |]) |> Array.ofList |> box
        "result", serializeType (res [for ma in args  -> ma.Type, None]).Value ] |> JsInterop.createObj 
  | Type.List(t) -> 
      [ "kind", box "array"
        "type", serializeType t ] |> JsInterop.createObj
  | Type.Object(obj) -> 
      [ yield "kind", box "object"
        match obj with
        | :? FSharpProvider.GenericType as gt -> 
            yield "generics", gt.TypeArguments |> Seq.map serializeType |> Array.ofSeq |> box
        | _ -> ()
        yield "members", obj.Members |> Array.map (fun m -> m.Name) |> box ] 
      |> JsInterop.createObj

let rec serializeEntity (rng:Range option) (ent:Entity) =
  let kind, extras = 
    match ent.Kind with
    | EntityKind.Root -> "root", []
    | EntityKind.Program _ -> "program", []
    | EntityKind.RunCommand _ -> "do", []
    | EntityKind.LetCommand _ -> "let", []
    | EntityKind.Operator _ -> "operator", []
    | EntityKind.List _ -> "list", []
    | EntityKind.Constant _ -> "constant", []
    | EntityKind.Function _ -> "function", []
    | EntityKind.GlobalValue _ -> "global", []
    | EntityKind.Variable _ -> "variable", []
    | EntityKind.Binding _ -> "binding", []
    | EntityKind.ArgumentList _ -> "args", []
    | EntityKind.CallSite _ -> "callsite", []
    | EntityKind.Member _ -> "member", []
    | EntityKind.MemberAccess _ -> "access", []
    | EntityKind.MemberName(n) -> "name", ["name", box n.Name]
    | EntityKind.NamedParam(n, _) -> "param", ["name", box n.Name]
    | EntityKind.Placeholder(n, _) -> "placeholder", ["name", box n.Name]
    | EntityKind.Call _ -> "call", []
  [ yield "kind", box kind
    if rng.IsSome then yield "range", JsInterop.createObj [ "start", box rng.Value.Start; "end", box rng.Value.End ]
    yield "getChildren", box (fun () -> ent.Antecedents |> List.toArray |> Array.map (serializeEntity None))
    yield "type", match ent.Type with Some t -> serializeType t | _ -> box "unknown" 
    yield! extras ]
  |> JsInterop.createObj

type checkingResult(_wellTyped:bool, _bindingResult:Binder.BindingResult, _program:Program) = 
  member x.wellTyped = _wellTyped
  member x.getEntities() = _bindingResult.Entities |> Array.map (fun (rng, ent) ->
    serializeEntity (Some rng) ent)

type editor(ed:monaco.editor.ICodeEditor) = 
  member x.getMonacoEditor() = ed
  member x.getValue() = 
    ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
  member x.setValue(text) = 
    ed.getModel().setValue(text)

type gamma(ctx:TheGammaContext) =
  static member createContext(providers:TheGammaProviders) =
    // Initialize and return services
    let checkingSvc = CheckingService("", providers.globals)
    gamma({ checkingService = checkingSvc; providers = providers })

  member x.check(code) = 
    async {
      let! res = ctx.checkingService.TypeCheck(code)
      return checkingResult(res) } |> thenable

  member x.evaluate(code, ?outputId) = 
    async {
      try return! evaluate ctx code outputId
      with e ->
        Log.exn("api", "Evaluating code '%O' failed with error '%O'.", code, e)
        return! raise e } |> thenable

  member x.errorsReported(f) = 
    ctx.checkingService.ErrorsReported.Add(fun (source, errors) ->
      errors 
      |> Array.sortBy (fun e -> e.Range.Start)
      |> Array.map (fun e -> 
          { number = e.Number; message = e.Message; 
            startLine = e.Range.Start.Line; startColumn = e.Range.Start.Column;
            endLine = e.Range.End.Line; endColumn = e.Range.End.Column }) 
      |> f )

  member x.createBlocks(id) = 
    Blocks.createBlockEditor ctx.checkingService ctx.providers.globals id

  member x.createEditor(id, source, options) =

    // Create editor using the size of the #id element, or size given by the user.
    // Store 'lineHeight', so that we can calculate size of editor when auto-sizing.
    let mutable lineHeight = 20.0
    let options = defaultArg options defaultEditorOptions 
    let el = document.getElementById(id)
    let width = defaultArg options.width el.clientWidth
    let height = defaultArg options.height el.clientHeight
    let maxHeight = defaultArg options.maxHeight (float System.Int32.MaxValue)
    let ed = Monaco.createMonacoEditor id source ctx.checkingService (fun opts ->
      opts.fontSize <- Some 15.0
      opts.lineHeight <- Some 20.0
      (defaultArg options.monacoOptions ignore) opts
      match opts.lineHeight with Some n -> lineHeight <- n | _ -> () )

    let dim = JsInterop.createEmpty<monaco.editor.IDimension>
    dim.width <- width
    dim.height <- height
    ed.layout(dim)

    let previewService = 
      if defaultArg options.enablePreview true then
        Some(PreviewService(ctx.checkingService, ctx.providers.globals, ed, previews))
      else None

    let mutable lastHeight = -1.0
    let autosizeEditor () =
      let text = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
      let lines = 1.0 + float (text.Split('\n').Length)
      let zoneHeight = match previewService with Some ps -> ps.ZoneHeight | _ -> 0.0
      let height = min maxHeight (max 200.0 (lines * 20.0 + zoneHeight))
      if height <> lastHeight then
        lastHeight <- height
        let dim = JsInterop.createEmpty<monaco.editor.IDimension>
        dim.width <- width
        dim.height <- height
        ed.layout(dim)
        el.style.height <- string dim.height + "px" 
        el.style.width <- string dim.width + "px" 

    if options.autoHeight = Some true then
      ed.getModel().onDidChangeContent(fun _ -> autosizeEditor ()) |> ignore     
      match previewService with
      | Some ps -> ps.ZoneSizeChanged.Add(fun _ -> autosizeEditor ())
      | _ -> ()
      autosizeEditor ()

    editor(ed)
  
type providers =
  static member createProviders(providers) =
    // Initialize type providers specified as key/values of the given object
    let globals = buildGlobalsTable (fun lookup -> async {
      let providers = JsHelpers.properties(providers) |> Array.map (fun kv ->
        (unbox<provider> kv.value) kv.key lookup)
      let! providers = Async.Parallel providers
      return Seq.concat providers })
    { globals = globals }    

  static member rest(url, ?cookies, ?ignoreFilter) : provider = 
    (fun name lookup -> async {
      let provider = 
        TypeProviders.RestProvider.provideRestType 
          lookup (resolveProvider lookup (defaultArg ignoreFilter false)) name url (defaultArg cookies "")
      return [ provider ] })

  static member library(url) : provider = 
    (fun _ lookup ->
      TypeProviders.FSharpProvider.provideFSharpTypes lookup url)

  static member pivot(url, ?ignoreFilter) : provider = 
    (fun name lookup -> async {
      let! t = TypeProviders.Pivot.providePivotType url (defaultArg ignoreFilter false) name lookup 
      return [t] })