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

Fable.Import.Node.require.Invoke("core-js") |> ignore

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

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
// Zones infra
// ------------------------------------------------------------------------------------------------

type PreviewService(checker:CheckingService, ed:monaco.editor.ICodeEditor, livePreviews) =
  let mutable currentZone : option<float * monaco.editor.IViewZone> = None
  let mutable zoneHeight = 0.0
  let mutable tree = JsInterop.createObj []
  let mutable container = document.createElement("div") :> Node

  let removeZone () =
    match currentZone with 
    | Some(id, _) -> ed.changeViewZones(fun accessor -> accessor.removeZone(id))
    | None -> ()
    currentZone <- None
  
  let createAndAddZone endLine =
    let mutable zoneId = -1.
    let zone = JsInterop.createEmpty<monaco.editor.IViewZone>
    
    let node = document.createElement_div()
    container <- document.createElement_div() :> Node
    tree <- JsInterop.createObj []    
    node.appendChild(container) |> ignore
    ed.changeViewZones(fun accessor ->  
      match currentZone with Some(id, _) -> accessor.removeZone(id) | _ -> ()
      zone.afterLineNumber <- endLine
      zone.heightInPx <- Some 300.0
      zone.domNode <- node
      zoneHeight <- 300.0
      zoneId <- accessor.addZone(zone) 
      currentZone <- Some (zoneId, zone) )

  let updateZones trigger liveState =
    let dom = 
      liveState.CurrentPreview |> FsOption.bind (fun p ->
        p.Render trigger liveState )
    match dom with 
    | None -> removeZone ()
    | Some prev ->
        if currentZone.IsNone then createAndAddZone 0.0
        Log.trace("live", "Render %O to zone %O", dom, currentZone)
        let id, zone = currentZone.Value
        let newTree = prev.Preview |> renderVirtual 
        let patches = Virtualdom.diff tree newTree
        container <- Virtualdom.patch container patches
        tree <- newTree
        let newHeight = (container :?> HTMLElement).clientHeight
        if zoneHeight <> newHeight || zone.afterLineNumber <> float prev.Line then
          zone.afterLineNumber <- float prev.Line
          zone.heightInPx <- Some newHeight
          zoneHeight <- newHeight
          ed.changeViewZones(fun accessor -> accessor.layoutZone(id) )

  let mutable lastCode = ""
  let mutable lastMapper = LocationMapper("")
  let mutable changingEditor = false

  let getUpdateEventAfterChange () = async {
    let code = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
    let position = ed.getPosition()
    if code <> lastCode then
      lastCode <- code
      lastMapper <- LocationMapper(code)
      let loc = lastMapper.LineColToAbsolute(int position.lineNumber, int position.column)
      let! _, _, program = checker.TypeCheck(code)
      return (UpdateSource(code, loc, program, lastMapper)) 
    else 
      let loc = lastMapper.LineColToAbsolute(int position.lineNumber, int position.column)
      return (UpdateLocation(loc)) }

  let createLivePreview (ed:monaco.editor.ICodeEditor) = 
    let liveEvent = new Event<LiveEvent<CustomLiveEvent>>()
    let noState = { new CustomLiveState }
    let mutable liveState = 
      { Mapper = LocationMapper("")
        Location = 0
        Program = { Body = Ast.node { Start = 0; End = 0 } [] }
        Globals = []
        Code = ""
        Selection = None
        State = noState
        CurrentPreview = None }
    (*
    let mutable pivotState = 
      { Selection = None
        Focus = None
        Mapper = LocationMapper("")
        Location = 0
        Program = { Body = Ast.node { Start = 0; End = 0 } [] }
        Code = ""
        Globals = []
        Body = None
        Menus = Hidden }
        *) 

    let applyEvent evt =
      let liveState = updateLiveState liveState evt        
      let newState =
        match liveState.CurrentPreview with 
        | Some(prev) -> prev.Update liveEvent.Trigger liveState evt
        | None -> None
      match newState, evt with
      | Some newState, _ -> newState
      | _, (UpdateSource _ | UpdateLocation _) ->
        let state = livePreviews |> Seq.tryPick (fun lp ->
          let state = { liveState with CurrentPreview = Some lp; State = lp.InitialState }
          lp.Update liveEvent.Trigger state evt)
        defaultArg state { liveState with CurrentPreview = None; State = noState }
      | _ -> { liveState with CurrentPreview = None; State = noState }

    liveEvent.Publish.Add(fun evt ->
      try
        Log.trace("live", "Updating state %O with event %O", liveState, evt)
        let oldState = liveState 
        liveState <- applyEvent evt

        if (match evt with UpdateSource _ -> false | _ -> true) && (oldState.Code <> liveState.Code) then
          changingEditor <- true
          ed.getModel().setValue(liveState.Code)
        match liveState.Selection with
        | Some rng ->
            changingEditor <- true
            let mrng = JsInterop.createEmpty<monaco.IRange>
            mrng.startColumn <- float rng.StartColumn
            mrng.startLineNumber <- float rng.StartLineNumber
            mrng.endColumn <- float rng.EndColumn
            mrng.endLineNumber <- float rng.EndLineNumber
            ed.setSelection(mrng)
            liveState <- { liveState with Selection = None }
        | _ -> ()

        if changingEditor = true then
          changingEditor <- false
          async { 
            Log.trace("live", "Editor changed. Getting after change event...")
            let! evt = getUpdateEventAfterChange ()
            Log.trace("live", "Editor changed. Updating state %O with event %O", liveState, evt)
            liveState <- applyEvent evt
            Log.trace("live", "Editor changed. New state %O", liveState)
            updateZones liveEvent.Trigger liveState

            (*match pivotState.Focus with 
            | Some(focus, sel) ->
                Log.trace("live", "Set focus to element #%s", focus)
                pivotState <- { pivotState with Focus = None }
                let element = document.getElementById(focus) |> unbox<HTMLInputElement>
                element.focus()
                sel |> FsOption.iter (fun s -> element.selectionStart <- float s; element.selectionEnd <- float s)
            | _ -> ()*) } |> Async.StartImmediate
        else
          updateZones liveEvent.Trigger liveState
      with e ->
        Log.exn("live", "Error when updating state %O with event %O: %O", liveState, evt, e) )

    async { let! glob = globalTypes |> Async.AwaitFuture 
            liveEvent.Trigger(InitializeGlobals glob) } |> Async.StartImmediate

    liveEvent.Trigger

  let trigger = createLivePreview ed    
      
  do
    ed.onDidChangeCursorPosition(fun ce -> 
      if not changingEditor then
        let code = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
        Log.trace("live", "Cursor position changed: code <> lastCode = %s", code <> lastCode)
        async { let! evt = getUpdateEventAfterChange ()
                trigger evt } |> Async.StartImmediate ) |> ignore

// ------------------------------------------------------------------------------------------------
// Putting everything togeter
// ------------------------------------------------------------------------------------------------


[<Emit("setRunner($0, $1)")>]
let setRunner (article:string) (f:unit -> unit) = failwith "JS"

[<Emit("shareSnippet($0, $1)")>]
let shareSnippet (snippet:string) (compiled:string) = failwith "JS"

[<Emit("cannotShareSnippet()")>]
let cannotShareSnippet () = failwith "JS"

let callShowMethod outId (cmd:Node<_>) = async {
  match cmd.Node with
  | Command.Expr({ Entity = Some { Type = Some typ } } as inst) ->
      match Types.reduceType typ with
      | Type.Object { Members = members } ->
          let hasShow = members |> Array.exists (function 
            | Member.Method(name="show"; arguments=[_, _, Type.Primitive PrimitiveType.String]) -> true
            | _ -> false)
          if hasShow then
            let rng = { Range.Start = cmd.Range.End; End = cmd.Range.End }
            let outExpr = Ast.node rng (Expr.String(outId))
            let args = [{ Argument.Name = None; Argument.Value = outExpr }]
            let expr = Ast.node rng (Expr.Call(Some inst, Ast.node rng { Name = "show" }, Ast.node rng args))
            return Ast.node cmd.Range (Command.Expr(expr))
          else 
            return cmd
      | _ -> return cmd
  | _ -> return cmd }

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

[<Emit("eval($0)")>]
let eval (s:string) : unit = ()

let previews = 
  [ Live.Pivot.preview |> unbox<LivePreview<CustomLiveState, CustomLiveEvent>> ]

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

    let resizeEditor (text:string) =
      let dim = JsInterop.createEmpty<monaco.editor.IDimension>
      dim.width <- parent.clientWidth - 40.0
      dim.height <- max 100.0 (20.0 + float (text.Split('\n').Length) * 20.0)
      ed.layout(dim)
      monacoEl.style.height <- string dim.height + "px" 

    ed.getModel().onDidChangeContent(fun _ ->
      let text = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
      if optionsVisible then
        editorService.UpdateSource(text) 
      resizeEditor text) |> ignore
      
    resizeEditor source
    PreviewService(checkingService, ed, previews) |> ignore
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
