module TheGamma.Services

open Fable.Import
open TheGamma.Html
open TheGamma.Editors
open TheGamma.Common

module FsOption = Microsoft.FSharp.Core.Option

// ------------------------------------------------------------------------------------------------
// Type checker
// ------------------------------------------------------------------------------------------------

type CheckingMessage = 
  | TypeCheck of code:string * AsyncReplyChannel<bool * Binder.BindingResult * Program>
  | IsWellTyped of code:string * AsyncReplyChannel<bool>

type Position = { Line:int; Column:int }
type LineRange = { Start:Position; End:Position }

let rec offsetToLocation lines offs lengths =
  match lengths with
  | l::lengths when offs <= l -> { Line = lines; Column = offs }
  | l::lengths -> offsetToLocation (lines+1) (offs-l-1) lengths
  | [] -> { Line = lines; Column = offs  } // error? out of range

let rangeToLoc lengths (rng:Range) = 
  { Start = offsetToLocation 1 rng.Start lengths
    End = offsetToLocation 1 rng.Start lengths }

type CheckingService(article, globals:Future<Entity list>) =
  let errorsReported = Control.Event<_>()
  let emptyProg = { Body = Ast.node { Start = 0; End = 0 } [] }
  let bindingContext =  
    async { 
      let! globals = globals |> Async.AwaitFuture
      return Binder.createContext globals article } |> Async.StartAsFuture

  let errorsToLineCol (code:string) errors = 
    let lengths = code.Split('\n') |> Array.toList |> List.map (fun l -> l.Length)
    errors |> Array.map (fun e -> 
      { Number = e.Number; Message = e.Message; Range = rangeToLoc lengths e.Range })

  let typeCheck code = async {
    let! globals = Async.AwaitFuture globals
    try
      let progSyntax, parseErrors = Parser.parseProgram code
      let! bindingContext = bindingContext |> Async.AwaitFuture
      let progEntity, boundEntities = Binder.bindProgram bindingContext progSyntax
      do! TypeChecker.typeCheckProgram globals boundEntities progEntity
      let typeErrors = TypeChecker.collectTypeErrors progEntity
      Log.trace("service", "Type checking completed")
      let errors = errorsToLineCol code (Array.append parseErrors typeErrors)
      return Some(progSyntax, boundEntities, errors)
    with e ->
      Log.exn("service", "Type checking failed: %O", e)
      return None }

  let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop lastCode lastResult = async {
      let! msg = inbox.Receive()
      match msg with
      | IsWellTyped(code, repl) ->
          let! tc = typeCheck code
          match tc with 
          | Some(_, _, errs) when errs.Length = 0 -> repl.Reply(true)
          | _ -> repl.Reply(false)
          return! loop lastCode lastResult

      | TypeCheck(code, repl) when code = lastCode ->
          Log.trace("service", "Returning previous result")
          repl.Reply(lastResult)
          return! loop lastCode lastResult

      | TypeCheck(code, repl) ->
          Log.trace("service", "Type checking source code")
          let! tc = typeCheck code
          match tc with 
          | Some(prog, ents, errors) ->
              errorsReported.Trigger(code, errors)            
              let result = (errors.Length = 0, ents, prog)
              repl.Reply(result)
              return! loop code result
          | None -> 
              repl.Reply((false, Binder.BindingResult [||], emptyProg))
              return! loop lastCode lastResult }
    
    loop "" (false, Binder.BindingResult [||], emptyProg))

  member x.ErrorsReported = errorsReported.Publish
  member x.TypeCheck(code) = agent.PostAndAsyncReply(fun ch -> TypeCheck(code, ch))
  member x.IsWellTyped(code) = agent.PostAndAsyncReply(fun ch -> IsWellTyped(code, ch))


// ------------------------------------------------------------------------------------------------
// Live previews
// ------------------------------------------------------------------------------------------------

open Fable.Core
open Fable.Helpers
open Fable.Import.Browser
open TheGamma.Live.Common

type PreviewService(checker:CheckingService, globals:Future<list<Entity>>, ed:monaco.editor.ICodeEditor, livePreviews) =

  let zoneSizeChanged = new Event<unit>()
  let mutable currentZone : option<float * monaco.editor.IViewZone> = None
  let mutable zoneHeight = 0.0
  let mutable tree = JsInterop.createObj []
  let mutable container = document.createElement("div") :> Node

  let removeZone () =
    match currentZone with 
    | Some(id, _) -> ed.changeViewZones(fun accessor -> accessor.removeZone(id))
    | None -> ()
    currentZone <- None
    zoneSizeChanged.Trigger()
  
  let createAndAddZone endLine =
    let mutable zoneId = -1.
    let zone = JsInterop.createEmpty<monaco.editor.IViewZone>
    
    let node = document.createElement_div()
    node.style.width <- "1000px"
    node.style.height <- "1000px"
    container <- document.createElement_div() :> Node    
    tree <- JsInterop.createObj []    
    node.appendChild(container) |> ignore
    ed.changeViewZones(fun accessor ->  
      match currentZone with Some(id, _) -> accessor.removeZone(id) | _ -> ()
      zone.afterLineNumber <- endLine
      zone.heightInPx <- Some 1.0
      zone.domNode <- node
      zoneHeight <- 1.0
      zoneId <- accessor.addZone(zone) 
      currentZone <- Some (zoneId, zone) )

  let updateZones trigger liveState =
    let dom = 
      liveState.CurrentPreview |> FsOption.bind (fun p ->
        p.Render trigger liveState )
    match dom with 
    | None -> removeZone ()
    | Some prev ->
        if currentZone.IsNone then createAndAddZone (float prev.Line)
        let id, zone = currentZone.Value
        let newTree = prev.Preview |> renderVirtual 
        let patches = Virtualdom.diff tree newTree
        container <- Virtualdom.patch container patches
        tree <- newTree

        let rec waitForActualHeight n = async {
          let newHeight = (container :?> HTMLElement).clientHeight
          if n = 10 || newHeight <> 0.0 then return newHeight
          else 
            do! Async.Sleep (n*n)
            return! waitForActualHeight (n+1) }

        async {
          let! newHeight = waitForActualHeight 1
          Log.trace("live", "Old height: %s, New height: %s", zoneHeight, newHeight)
          if zoneHeight <> newHeight || zone.afterLineNumber <> float prev.Line then
            zone.afterLineNumber <-   float prev.Line
            zone.heightInPx <- Some newHeight
            zoneHeight <- newHeight
            ed.changeViewZones(fun accessor -> accessor.layoutZone(id))
            zoneSizeChanged.Trigger() } |> Async.StartImmediate

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

    let applyEvent evt =
      let liveState = updateLiveState liveState evt        
      let newState =
        match liveState.CurrentPreview with 
        | Some(prev) -> prev.Update liveEvent.Trigger liveState evt
        | None -> None
      match newState, evt with
      | Some newState, _ -> newState
      | _, (UpdateSource _ | UpdateLocation _) ->
        Log.trace("live", "Searching for available previews")
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
            updateZones liveEvent.Trigger liveState } |> Async.StartImmediate
        else
          updateZones liveEvent.Trigger liveState
      with e ->
        Log.exn("live", "Error when updating state %O with event %O: %O", liveState, evt, e) )

    async { let! glob = globals |> Async.AwaitFuture 
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

  member x.ZoneSizeChanged = 
    zoneSizeChanged.Publish

  member x.ZoneHeight = 
    if currentZone <> None then zoneHeight 
    else 0.0