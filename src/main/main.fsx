#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../libraries/bin/Debug/libraries.dll"
#r "../thegamma/bin/Debug/thegamma.dll"
#r "../bindings/bin/Debug/bindings.dll"
open Fable.Core
open Fable.Core.Extensions
open Fable.Import
open Fable.Helpers
open Fable.Import.Browser

open TheGamma
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
  let lookupNamed n tyargs = 
    match named.TryFind(n) with
    | Some(r, tya) -> 
        if List.length tya <> List.length tyargs then 
          Log.error("Named type '%s' has mismatching length of type arguments", n)
          failwith (sprintf "Named type '%s' has mismatching length of type arguments" n)
        if tya.Length > 0 then 
          Type.App(r, tyargs)
        else r 
    | None -> 
        Log.error("Could not find named type '%s'", n)
        failwith (sprintf "Could not find named type '%s'" n)

  let! provided = provideTypes lookupNamed
  let allTypes = 
    [ // Pretend we support these - the names appear in the F# provided types
      // and if the functions are not actually used, providing Any type works 
      yield TypeProviders.NamedType("value", ["a"], Type.Any)
      yield TypeProviders.NamedType("object", [], Type.Any)
      yield TypeProviders.NamedType("seq", ["a"], Type.Any) 
      yield TypeProviders.NamedType("async", ["a"], Type.Any) 
      yield! provided ]

  // Build lookup table from named types and
  // list of global entities (provided global values)
  named <- 
    allTypes
    |> Seq.choose (function TypeProviders.NamedType(s, tya, t) -> Some(s, (t, tya)) | _ -> None)
    |> Map.ofSeq
  let globalEntities = allTypes |> List.choose (function 
    | TypeProviders.GlobalValue(n, m, e, t) -> 
        Some(Interpreter.globalEntity n m t (Some e))
    | _ -> None)
  return globalEntities } 

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
      match Types.reduceType typ with
      | Type.Object { Members = members } ->
          let hasShow = members |> Array.exists (function 
            | Member.Method(name="show"; arguments=[_, _, Type.Primitive PrimitiveType.String]) -> true
            | _ -> false)
          if hasShow then
            let rng = { Range.Start = cmd.Range.End; End = cmd.Range.End }
            let outExpr = Ast.node rng (Expr.String(outputId))
            let args = [{ Argument.Name = None; Argument.Value = outExpr }]
            let expr = Ast.node rng (Expr.Call(Some inst, Ast.node rng { Name = "show" }, Ast.node rng args))
            Ast.node cmd.Range (Command.Expr(expr))
          else cmd
      | _ -> cmd
  | _ -> cmd

[<Emit("eval($0)")>]
let eval (s:string) : unit = ()

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
  TheGamma.Maps.timeline<int, int>.create(s) |> ignore
  TheGamma.Series.series<int, int>.values([| 1 |]) |> ignore
  return eval code }

type provider = string -> (string -> Type list -> Type) -> Async<list<ProvidedType>>

let previews = 
  [ Live.Pivot.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> 
    Live.Showable.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> ]

type editorOptions =
  { width : float option
    height : float option
    maxHeight : float option
    autoHeight : bool option
    monacoOptions : (monaco.editor.IEditorConstructionOptions -> unit) option }

type error = 
  { number : int
    message : string
    startLine : int
    startColumn : int
    endLine : int
    endColumn : int }

let defaultEditorOptions = 
  { width = None; height = None; maxHeight = None; autoHeight = None; monacoOptions = None }

type editor(ed:monaco.editor.ICodeEditor) = 
  member x.getValue() = 
    ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)

type gamma(ctx:TheGammaContext) =
  static member createContext(providers:TheGammaProviders) =
    // Initialize and return services
    let checkingSvc = CheckingService("", providers.globals)
    gamma({ checkingService = checkingSvc; providers = providers })

  member x.evaluate(code, ?outputId) = 
    async {
      try do! evaluate ctx code outputId
      with e ->
        Log.exn("api", "Evaluating code '%O' failed with error '%O'.", code, e) }
    |> Async.StartImmediate

  member x.errorsReported(f) = 
    ctx.checkingService.ErrorsReported.Add(fun (source, errors) ->
      errors 
      |> Array.sortBy (fun e -> e.Range.Start)
      |> Array.map (fun e -> 
          { number = e.Number; message = e.Message; 
            startLine = e.Range.Start.Line; startColumn = e.Range.Start.Column;
            endLine = e.Range.End.Line; endColumn = e.Range.End.Column }) 
      |> f )

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

    let previewService = PreviewService(ctx.checkingService, ctx.providers.globals, ed, previews)

    let mutable lastHeight = -1.0
    let autosizeEditor () =
      let text = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
      let lines = 1.0 + float (text.Split('\n').Length)
      let height = min maxHeight (max 200.0 (lines * 20.0 + previewService.ZoneHeight))
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
      previewService.ZoneSizeChanged.Add(fun _ -> autosizeEditor ())
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

  static member rest(url, ?cookies) : provider = 
    (fun name lookup -> async {
      return [TypeProviders.RestProvider.provideRestType lookup name url (defaultArg cookies "")] })

  static member library(url) : provider = 
    (fun _ lookup ->
      TypeProviders.FSharpProvider.provideFSharpTypes lookup url)

  static member pivot(url) : provider = 
    (fun name lookup -> async {
      let! membersJson = Http.Request("GET", Pivot.concatUrl url "metadata")
      let members = JsHelpers.properties(jsonParse<obj> membersJson) |> Array.map (fun kv -> 
        let typ = 
          match unbox kv.value with
          | "string" -> PrimitiveType.String
          | "bool" -> PrimitiveType.Bool
          | "number" -> PrimitiveType.Number
          | s -> failwith (sprintf "The property '%s' has invalid type '%s'. Only 'string', 'number' and 'bool' are supported." kv.key s)
        kv.key, typ)
      return [TypeProviders.Pivot.providePivotType url name lookup members] })