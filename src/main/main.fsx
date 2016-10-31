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

//Fable.Import.Node.require.Invoke("core-js") |> ignore

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------
(*
TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics1" (services + "olympics") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics3" (services + "pivot") ("source=" + services + "olympics")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy1" (services + "smlouvy") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy2" (services + "pivot") ("source=" + services + "smlouvy")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      TypeProviders.Pivot.providePivotType (services + "pdata/olympics") "olympics" lookupNamed
        [ "Games", PrimitiveType.String; "Year", PrimitiveType.Number;  "Sport", PrimitiveType.String; "Discipline", PrimitiveType.String 
          "Athlete", PrimitiveType.String; "Team", PrimitiveType.String; "Gender", PrimitiveType.String; "Event", PrimitiveType.String 
          "Medal", PrimitiveType.String; "Gold", PrimitiveType.Number; "Silver", PrimitiveType.Number; "Bronze", PrimitiveType.Number ]
      
      TypeProviders.Pivot.providePivotType (services + "pdata/smlouvy") "smlouvy" lookupNamed
        [ "Uzavřeno", PrimitiveType.String; "Publikováno", PrimitiveType.String; "Hodnota", PrimitiveType.Number
          "Chybí hodnota", PrimitiveType.String; "Subjekt", PrimitiveType.String; "Útvar", PrimitiveType.String
          "Schválil", PrimitiveType.String; "Předmět", PrimitiveType.String; "Odkaz", PrimitiveType.String
          "Platnost", PrimitiveType.String; "Příjemci", PrimitiveType.String; "Příjemci (IČO)", PrimitiveType.String ]            
*)

// TypeProviders.FSharpProvider.provideFSharpTypes lookupNamed ("/ext/libraries.json?" + string System.DateTime.Now.Ticks)     

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


(*
let services = 
  if isLocalHost() then "http://127.0.0.1:10042/"
  else "http://thegamma-services.azurewebsites.net/"

type ProvidedTypes = 
  { LookupNamed : string -> Type list -> Type
    Globals : list<string * Metadata list * Babel.Expression * Type> }
    
let types = async {
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

  let restTys = 
    [ TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics1" (services + "olympics") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics3" (services + "pivot") ("source=" + services + "olympics")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy1" (services + "smlouvy") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy2" (services + "pivot") ("source=" + services + "smlouvy")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      TypeProviders.Pivot.providePivotType (services + "pdata/olympics") "olympics" lookupNamed
        [ "Games", PrimitiveType.String; "Year", PrimitiveType.Number;  "Sport", PrimitiveType.String; "Discipline", PrimitiveType.String 
          "Athlete", PrimitiveType.String; "Team", PrimitiveType.String; "Gender", PrimitiveType.String; "Event", PrimitiveType.String 
          "Medal", PrimitiveType.String; "Gold", PrimitiveType.Number; "Silver", PrimitiveType.Number; "Bronze", PrimitiveType.Number ]
      
      TypeProviders.Pivot.providePivotType (services + "pdata/smlouvy") "smlouvy" lookupNamed
        [ "Uzavřeno", PrimitiveType.String; "Publikováno", PrimitiveType.String; "Hodnota", PrimitiveType.Number
          "Chybí hodnota", PrimitiveType.String; "Subjekt", PrimitiveType.String; "Útvar", PrimitiveType.String
          "Schválil", PrimitiveType.String; "Předmět", PrimitiveType.String; "Odkaz", PrimitiveType.String
          "Platnost", PrimitiveType.String; "Příjemci", PrimitiveType.String; "Příjemci (IČO)", PrimitiveType.String ]            

      // TODO: some more types 
      TypeProviders.NamedType("value", ["a"], Type.Any)
      TypeProviders.NamedType("object", [], Type.Any)
      TypeProviders.NamedType("seq", ["a"], Type.Any) 
      TypeProviders.NamedType("async", ["a"], Type.Any) ]

  let! fsTys = TypeProviders.FSharpProvider.provideFSharpTypes lookupNamed ("/ext/libraries.json?" + string System.DateTime.Now.Ticks)     
  let allTys = restTys @ fsTys

  named <- 
    allTys 
    |> Seq.choose (function TypeProviders.NamedType(s, tya, t) -> Some(s, (t, tya)) | _ -> None)
    |> Map.ofSeq

  let globals = 
    allTys 
    |> List.choose (function TypeProviders.GlobalValue(s, m, e, t) -> Some(s, m, e, t) | _ -> None)
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsNamedFuture "types"

let globalTypes = async { 
  let! ty = types |> Async.AwaitFuture
  Log.trace("typechecker", "Global values: %O", Array.ofList ty.Globals)
  return ty.Globals |> List.map (fun (n, m, e, t) -> Interpreter.globalEntity n m t (Some e)) } |> Async.StartAsNamedFuture "global types"

let globalExprs = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> List.map (fun (n, _, e, _) -> n, e) |> Map.ofList } |> Async.StartAsNamedFuture "global exps"

// ------------------------------------------------------------------------------------------------
// HTML helpers
// ------------------------------------------------------------------------------------------------

let findElements f (el:Element) =
  let rec loop acc (el:Node) = 
    if el = null then acc
    else
      let acc = 
        if el.nodeType = 1.0 && f (el :?> Element) then (el :?> Element)::acc
        else acc
      loop (loop acc el.firstChild) el.nextSibling
  loop [] el.firstChild

let tryFindChildElement f (el:Element) = 
  let rec loop (el:Node) = 
    if el = null then None
    elif el.nodeType = 1.0 && f (el :?> HTMLElement) then Some (el :?> HTMLElement)
    else 
      match loop el.firstChild with
      | None -> loop el.nextSibling
      | res -> res  
  loop el.firstChild 

let findChildElement f e = tryFindChildElement f e |> FsOption.get

let withClass cls (el:Element) = el.classList.contains cls


// ------------------------------------------------------------------------------------------------
// Putting everything togeter
// ------------------------------------------------------------------------------------------------


[<Emit("setRunner($0, $1)")>]
let setRunner (article:string) (f:unit -> unit) = failwith "JS"

[<Emit("shareSnippet($0, $1)")>]
let shareSnippet (snippet:string) (compiled:string) = failwith "JS"

[<Emit("cannotShareSnippet()")>]
let cannotShareSnippet () = failwith "JS"


let renderErrors article el (source, errors) = 
  if not (Seq.isEmpty errors) then
    Log.event("compiler", "errors", article, 
      JsInterop.createObj ["source", box source; "errors", box [| for e in errors -> e.Number |] ])
  h?ul["class" => "error"] 
    [ for e in errors |> Seq.sortBy (fun e -> e.Range.Start) -> 
        h?li [] [
          h?span ["class" => "err"] [ text (sprintf "error %d" e.Number) ]
          text " "
          h?span ["class" => "loc"] [ text (sprintf "at line %d col %d" e.Range.Start.Line e.Range.Start.Column) ]
          text (": " + e.Message) ] ]
  |> renderTo el


let previews = 
  [ Live.Pivot.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> 
    Live.Showable.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> ]

let setupEditor (parent:HTMLElement) =
  let source = (findChildElement (withClass "ia-source") parent).innerText.Trim()
  let compiled = tryFindChildElement (withClass "ia-compiled") parent |> FsOption.map (fun el -> el.innerText.Trim())
  let outputId = (findChildElement (withClass "ia-output") parent).id
    
  let runBtn = findChildElement (withClass "ia-run") parent
  let shareBtn = findChildElement (withClass "ia-share") parent
  let showCodeBtn = findChildElement (withClass "ia-show-source") parent
  let showOptionsBtn = tryFindChildElement (withClass "ia-show-options") parent
  
  let editorEl = findChildElement (withClass "ia-editor") parent
  let monacoEl = findChildElement (withClass "ia-monaco") parent
  let errorsEl = findChildElement (withClass "ia-errors") parent
  let optionsEl = findChildElement (withClass "ia-options") parent
  
  let article = parent.dataset.["article"]

  let checkingService = CheckingService(article, globalTypes)
  let editorService = EditorService(article, checkingService.TypeCheck, 2000)
  checkingService.ErrorsReported.Add (renderErrors article errorsEl)

  let run text = async {
    Log.event("compiler", "run", article, text)
    let! code = async {
      match compiled with
      | Some compiled when text = source -> return compiled
      | _ ->
        let! _, _, prog = checkingService.TypeCheck(text)
        let! newBody = prog.Body.Node |> Async.map (callShowMethod outputId)
        let prog = { prog with Body = { prog.Body with Node = newBody } }
        return! CodeGenerator.compileAndRun globalExprs text prog }

    // Get fable to reference everything
    let s = TheGamma.Series.series<int, int>.create(async { return [||] }, "", "", "") 
    TheGamma.TypeProvidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
    TypeProvidersRuntime.trimLeft |> ignore
    TheGamma.GoogleCharts.chart.bar |> ignore
    TheGamma.table<int, int>.create(s) |> ignore
    TheGamma.Maps.timeline<int, int>.create(s) |> ignore
    TheGamma.Series.series<int, int>.values([| 1 |]) |> ignore
    eval code }

  setRunner article (fun () -> 
    run source |> Async.StartImmediate)

  let mutable optionsVisible = false
  let mutable editorVisible = false

  let ed = Lazy.Create(fun () ->   
    let ed = Monaco.createMonacoEditor monacoEl.id source (fun opts ->
      opts.fontFamily <- Some "Inconsolata"
      opts.fontSize <- Some 15.0
      opts.lineHeight <- Some 20.0 )

    let previewService = PreviewService(checkingService, globalTypes, ed, previews)

    let resizeEditor (text:string) =
      let dim = JsInterop.createEmpty<monaco.editor.IDimension>
      dim.width <- parent.clientWidth - 40.0
      dim.height <- max 100.0 (20.0 + float (text.Split('\n').Length) * 20.0 + previewService.ZoneHeight)
      ed.layout(dim)
      monacoEl.style.height <- string dim.height + "px" 

    ed.getModel().onDidChangeContent(fun _ ->
      let text = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
      if optionsVisible then
        editorService.UpdateSource(text) 
      resizeEditor text) |> ignore
     
    previewService.ZoneSizeChanged.Add(fun _ ->
      let text = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
      resizeEditor text )
    
    resizeEditor source
    ed )
  
  let getText() = 
    if not ed.IsValueCreated then source
    else ed.Value.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)

  let setText (edit:string) membr t = 
    Log.event("options", "set-text", article, JsInterop.createObj ["edit", box edit; "member", box membr ])
    ed.Value.getModel().setValue(t)
    if showOptionsBtn.IsSome && optionsVisible then
      editorService.UpdateSource(t, true)
    run(t) |> Async.StartImmediate

  let showOrHideActions () =
    let vis = if optionsVisible || editorVisible then "inline" else "none"
    let modf = getText() <> source
    runBtn.style.display <- vis
    shareBtn.style.display <- if modf then "inline" else vis

  showOptionsBtn |> FsOption.iter (fun btn -> 
    editorService.EditorsUpdated.Add (fun eds ->
      eds
      |> List.sortBy (fun ed -> ed.Range.Start)
      |> List.map (Editors.renderEditor checkingService.IsWellTyped setText (getText())) 
      |> h?div ["class" => "ia-editor-panel"]
      |> renderTo optionsEl )
  
    btn.onclick <- fun _ ->
      optionsVisible <- not optionsVisible
      showOrHideActions()
      optionsEl.style.display <- if optionsVisible then "block" else "none"
      Log.event("gui", "options", article, box optionsVisible)
      if optionsVisible then editorService.UpdateSource(getText())
      box () )

  let switchEditor () =
    editorVisible <- not editorVisible
    showOrHideActions()
    editorEl.style.display <- if editorVisible then "block" else "none"
    Log.event("gui", "editor", article, editorVisible)
    if editorVisible then 
      ed.Force() |> ignore
      editorService.UpdateSource(getText()) 
    
  showCodeBtn.onclick <- fun _ -> switchEditor(); box()
  if source.Contains("empty.create") then switchEditor()

  shareBtn.onclick <- fun e -> 
    let text = getText()
    Log.event("gui", "share", article, text)
    async { 
      let! ok, _, prog = checkingService.TypeCheck(text)
      let! newBody = prog.Body.Node |> Async.map (callShowMethod "output-id-placeholder")
      let prog = { prog with Body = { prog.Body with Node = newBody } }
      let! compiled = CodeGenerator.compileAndRun globalExprs text prog         
      if not ok then cannotShareSnippet()
      else shareSnippet text compiled } |> Async.StartImmediate
    box ()

  runBtn.onclick <- fun e -> 
    Log.event("gui", "run", article, "click")
    getText() |> run |> Async.StartImmediate |> box

  ed, checkingService

let servicesLookup = ResizeArray<Lazy<monaco.editor.ICodeEditor> * _>()

Monaco.setupMonacoServices(fun name ->
  servicesLookup |> Seq.pick (fun (ed, svc) ->
    if ed.IsValueCreated && ed.Value.getModel().uri.toString() = name then Some(svc)
    else None )
)

for el in findElements (withClass "ia-figure") document.body do
  servicesLookup.Add(setupEditor (el :?> HTMLElement))
*)

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

(*
TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics1" (services + "olympics") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "olympics3" (services + "pivot") ("source=" + services + "olympics")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy1" (services + "smlouvy") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "smlouvy2" (services + "pivot") ("source=" + services + "smlouvy")
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypeProviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      TypeProviders.Pivot.providePivotType (services + "pdata/olympics") "olympics" lookupNamed
        [ "Games", PrimitiveType.String; "Year", PrimitiveType.Number;  "Sport", PrimitiveType.String; "Discipline", PrimitiveType.String 
          "Athlete", PrimitiveType.String; "Team", PrimitiveType.String; "Gender", PrimitiveType.String; "Event", PrimitiveType.String 
          "Medal", PrimitiveType.String; "Gold", PrimitiveType.Number; "Silver", PrimitiveType.Number; "Bronze", PrimitiveType.Number ]
      
      TypeProviders.Pivot.providePivotType (services + "pdata/smlouvy") "smlouvy" lookupNamed
        [ "Uzavřeno", PrimitiveType.String; "Publikováno", PrimitiveType.String; "Hodnota", PrimitiveType.Number
          "Chybí hodnota", PrimitiveType.String; "Subjekt", PrimitiveType.String; "Útvar", PrimitiveType.String
          "Schválil", PrimitiveType.String; "Předmět", PrimitiveType.String; "Odkaz", PrimitiveType.String
          "Platnost", PrimitiveType.String; "Příjemci", PrimitiveType.String; "Příjemci (IČO)", PrimitiveType.String ]            
*)
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

let defaultEditorOptions = 
  { width = None; height = None; maxHeight = None; autoHeight = None; monacoOptions = None }

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

  static member pivot(url, members) : provider = 
    let members = JsHelpers.properties(members) |> Array.map (fun kv -> 
      let typ = 
        match unbox kv.value with
        | "string" -> PrimitiveType.String
        | "bool" -> PrimitiveType.Bool
        | "number" -> PrimitiveType.Number
        | s -> failwith (sprintf "The property '%s' has invalid type '%s'. Only 'string', 'number' and 'bool' are supported." kv.key s)
      kv.key, typ)
    (fun name lookup -> async {
      return [TypeProviders.Pivot.providePivotType url name lookup members] })