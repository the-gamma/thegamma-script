#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../libraries/bin/Debug/libraries.dll"
#r "../thegamma/bin/Debug/thegamma.dll"
#r "../bindings/bin/Debug/bindings.dll"
#r "../gui/bin/Debug/gui.dll"
open Fable.Core.Extensions
open Fable.Import
open Fable.Import.Browser
module FsOption = Microsoft.FSharp.Core.Option

open TheGamma
open TheGamma.Html
//open TheGamma.Babel
open TheGamma.Common
open TheGamma.TypeChecker
open Fable.Core
 
Fable.Import.Node.require.Invoke("core-js") |> ignore

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

let services = 
  if isLocalHost() then "http://127.0.0.1:10042/"
  else "http://thegamma-services.azurewebsites.net/"

type ProvidedTypes = 
  { LookupNamed : string -> Type list -> Type
    Globals : list<string * Babel.Expression * Type> }
    
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
    [ TypePoviders.RestProvider.provideRestType lookupNamed 
        "olympics1" (services + "olympics") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "olympics" (services + "pivot") ("source=" + services + "olympics")
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "smlouvy1" (services + "smlouvy") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "smlouvy2" (services + "pivot") ("source=" + services + "smlouvy")
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      TypeProviders.Pivot.providePivotType (services + "pdata/olympics") "olympics2" lookupNamed
        [ "Games", PrimitiveType.String; "Year", PrimitiveType.Number;  "Sport", PrimitiveType.String; "Discipline", PrimitiveType.String 
          "Athlete", PrimitiveType.String; "Team", PrimitiveType.String; "Gender", PrimitiveType.String; "Event", PrimitiveType.String 
          "Medal", PrimitiveType.String; "Gold", PrimitiveType.Number; "Silver", PrimitiveType.Number; "Bronze", PrimitiveType.Number ]
      
      TypeProviders.Pivot.providePivotType (services + "pdata/smlouvy") "smlouvy" lookupNamed
        [ "Uzavřeno", PrimitiveType.String; "Publikováno", PrimitiveType.String; "Hodnota", PrimitiveType.Number
          "Chybí hodnota", PrimitiveType.String; "Subjekt", PrimitiveType.String; "Útvar", PrimitiveType.String
          "Schválil", PrimitiveType.String; "Předmět", PrimitiveType.String; "Odkaz", PrimitiveType.String
          "Platnost", PrimitiveType.String; "Příjemci", PrimitiveType.String; "Příjemci (IČO)", PrimitiveType.String ]            

      // TODO: some more types 
      TypePoviders.NamedType("value", ["a"], Type.Any)
      TypePoviders.NamedType("seq", ["a"], Type.Any) 
      TypePoviders.NamedType("async", ["a"], Type.Any) ]

  let! fsTys = TypePoviders.FSharpProvider.provideFSharpTypes lookupNamed ("/ext/libraries.json?" + string System.DateTime.Now.Ticks)     
  let allTys = restTys @ fsTys

  named <- 
    allTys 
    |> Seq.choose (function TypePoviders.NamedType(s, tya, t) -> Some(s, (t, tya)) | _ -> None)
    |> Map.ofSeq

  let globals = 
    allTys 
    |> List.choose (function TypePoviders.GlobalValue(s, e, t) -> Some(s, e, t) | _ -> None)
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsNamedFuture "types"

let globalTypes = async { 
  let! ty = types |> Async.AwaitFuture
  Log.trace("typechecker", "Global values: %O", Array.ofList ty.Globals)
  return ty.Globals |> List.map (fun (n, e, t) -> Interpreter.globalEntity n t (Some e)) } |> Async.StartAsNamedFuture "global types"

let globalExprs = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> List.map (fun (n, e, _) -> n, e) |> Map.ofList } |> Async.StartAsNamedFuture "global exps"

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
// More experiments
// ------------------------------------------------------------------------------------------------

open TheGamma.Ast
open TheGamma.Services
open TheGamma.Common

let pickMetaByType ctx typ metas = 
  metas |> List.tryPick (fun m -> 
    if m.Context = ctx && m.Type = typ then Some(m.Data)
    else None)

let pickPivotChainElement expr =
  match expr.Entity with
  | Some { Kind = EntityKind.ChainElement _; Meta = m } -> 
      match pickMetaByType "http://thegamma.net" "Pivot" m with
      | Some m -> Some(unbox<TypeProviders.Pivot.Transformation list> m)
      | _ -> None
  | Some { Kind = EntityKind.GlobalValue _; Meta = m } -> 
      Some([])
  | _ -> None

let tryFindPreview globals (ent:Entity) = 
  let nm = {Name.Name="preview"}
  match ent.Type with 
  | Some(Type.Object(TypeChecker.FindProperty nm prev)) ->
      let res = Interpreter.evaluate globals ent  
      match res with
      | Some { Preview = Some p } ->
          Some(fun id ->
            table<int, int>.create(unbox<Series.series<string, obj>> p).set(showKey=false).show(id)
          )
      | _ -> None
      //Log.trace("system", "Preview rendered")
      //Some(sprintf "<ul style='font-size:10pt'>%s</ul>" (String.concat "" s))
      //Some(sprintf "preview :-) %A" res)
  | _ ->
      None //Some("no preview :-(")

let commandAtLocation loc (program:Program) =
  program.Body.Node |> List.tryFind (fun cmd ->
    cmd.Range.Start <= loc && cmd.Range.End + 1 >= loc)
(*
let chainElementAtLocation loc (ents:Binder.BindingResult) =
  let chainElements = 
    ents.Entities |> Array.choose (fun (rng, ent) ->
      match ent.Kind with
      | EntityKind.ChainElement(name=n) when rng.Start <= loc && rng.End >= loc -> Some(rng, ent)
      | _ -> None)
  if chainElements.Length > 0 then
    Some(chainElements |> Array.minBy (fun (rng, _) -> rng.End))
  else None
*)
let transformName = function
  | TypeProviders.Pivot.Transformation.DropColumns _ -> "drop columns"
  | TypeProviders.Pivot.Transformation.Empty _ -> "empty"
  | TypeProviders.Pivot.Transformation.FilterBy _ -> "filter by"
  | TypeProviders.Pivot.Transformation.GetSeries _ -> "get series"
  | TypeProviders.Pivot.Transformation.GetTheData _ -> "get the data"
  | TypeProviders.Pivot.Transformation.GroupBy _ -> "group by"
  | TypeProviders.Pivot.Transformation.Paging _ -> "paging"
  | TypeProviders.Pivot.Transformation.SortBy _ -> "sort by"

open TheGamma.TypeProviders

type PivotSection = 
  { Transformation : Pivot.Transformation   
    Nodes : Node<Expr> list }

let createPivotSections tfss = 
  let rec loop acc (currentTfs, currentEnts, currentLength) = function
    | (e, tfs)::tfss when 
          transformName (List.head tfs) = transformName currentTfs && 
          List.length tfs = currentLength ->
        loop acc (List.head tfs, e::currentEnts, currentLength) tfss
    | (e, tfs)::tfss ->
          let current = { Transformation = currentTfs; Nodes = List.rev currentEnts }
          loop (current::acc) (List.head tfs, [e], List.length tfs) tfss
    | [] -> 
          let current = { Transformation = currentTfs; Nodes = List.rev currentEnts }
          List.rev (current::acc)
    
  let tfss = tfss |> List.choose (fun node ->
    match pickPivotChainElement node with
    | Some(tfs) ->
        let tfs = tfs |> List.filter (function Pivot.Empty -> false | _ -> true)
        if List.isEmpty tfs then None else Some(node, tfs)
    | None -> None )
  match tfss with
  | (e, tfs)::tfss -> loop [] (List.head tfs, [e], List.length tfs) tfss
  | [] -> []

let rec collectChain acc node =
  match node.Node with
  | Expr.Call(Some e, n, _) 
  | Expr.Property(e, n) -> collectChain ((n.Range.Start, node)::acc) e
  | Expr.Variable(n) -> (n.Range.Start, node)::acc
  | _ -> acc

let rec collectFirstChain expr = 
  let chain = collectChain [] expr 
  if not (List.isEmpty chain) then Some chain else
  match expr with
  | { Node = ExprNode(es, _) } -> es |> List.tryPick collectFirstChain
  | _ -> None

// ------------------------------------------------------------------------------------------------
// Elmish pivot editor
// ------------------------------------------------------------------------------------------------

type PivotEditorAction = 
  | InitializeGlobals of seq<Entity>
  | UpdateSource of string * int * Program * Monaco.LocationMapper
  | UpdateLocation of int
  | Select of (int * int) * (int * int)
  | AddTransform of Pivot.Transformation
  | OpenAddDropdown
  | HideAddDropDown

type PivotEditorMenus =
  | AddDropdownOpen
  | Hidden

type PivotEditorState = 
  { // Initialized once - global values
    Globals : seq<Entity>
    // Updated when code changes - parsed program
    Code : string
    Program : Program
    Mapper : Monaco.LocationMapper
    // Updated when cursor moves 
    Location : int

    // Calculated from the above
    Body : Node<Expr> option
    Selection : option<monaco.IRange>
    Menus : PivotEditorMenus  }

let updateBody state = 
  match commandAtLocation state.Location state.Program with
  | Some(cmd) ->
      let line, col = state.Mapper.AbsoluteToLineCol(cmd.Range.End + 1)
      let (Command.Expr expr | Command.Let(_, expr)) = cmd.Node 
      { state with Body = Some expr }
  | _ -> 
      { state with Body = None }

let hideMenus state = { state with Menus = Hidden }

let updatePivotState state event = 
  match event with
  | InitializeGlobals(globals) ->
      { state with PivotEditorState.Globals = globals }
  | UpdateLocation(loc) ->
      { state with Location = loc } |> updateBody |> hideMenus
  | UpdateSource(code, loc, program, mapper) ->
      { state with Location = loc; Program = program; Code = code; Mapper = mapper } |> updateBody |> hideMenus
  | HideAddDropDown ->
      hideMenus state
  | OpenAddDropdown ->
      { state with Menus = AddDropdownOpen }
  | AddTransform tfs ->
      match state.Body with
      | Some body ->
          match collectFirstChain body with
          | Some chain ->
              for _, n in chain do
                match pickPivotChainElement n with
                | Some (Pivot.GetSeries _ :: _) 
                | Some (Pivot.GetTheData _ :: _) 
                | None -> ()
                | Some tfs -> 
                    Log.trace("live", "Chain element [%s-%s]: %s", n.Range.Start, n.Range.End, n.Entity.Value.Name)
          | _ -> ()
      | _ -> ()
      hideMenus state
  | Select((sl, sc), (el, ec)) ->        
      let rng = JsInterop.createEmpty<monaco.IRange>
      rng.startLineNumber <- float sl
      rng.startColumn <- float sc
      rng.endLineNumber <- float el
      rng.endColumn <- float ec
      { state with Selection = Some rng } |> hideMenus

let renderPivot trigger state = 
  let trigger action = fun _ _ -> trigger action
  match state.Body with
  | None -> None 
  | Some body ->
  match collectFirstChain body with
  | None -> None
  | Some(chainNodes) ->
      let starts = [| for r,n in chainNodes -> sprintf "%d: %s" r n.Entity.Value.Name |]
      Log.trace("live", "Find chain element at %d in %O", state.Location, starts)
      match chainNodes |> List.filter (fun (start, node) -> state.Location >= start) |> List.tryLast with
      | None -> None
      | Some(_, selNode) ->
          let selEnt = selNode.Entity.Value
          let sections = chainNodes |> List.map snd |> createPivotSections 
          let preview = defaultArg (tryFindPreview state.Globals selEnt) ignore
          let dom = 
            h?div [
                yield "class" => "pivot-preview"
                if state.Menus = AddDropdownOpen then
                  yield "click" =!> trigger HideAddDropDown 
              ] [
              h?ul ["class" => "tabs"] [
                for sec in sections ->
                  let selected = sec.Nodes |> List.exists (fun secEnt -> selEnt.Symbol = secEnt.Entity.Value.Symbol)
                  let identRange = 
                    match sec.Nodes with
                    | { Node = Expr.Variable n | Expr.Call(_, n, _) | Expr.Property(_, n) }::_ -> 
                        state.Mapper.AbsoluteToLineCol(n.Range.Start),
                        state.Mapper.AbsoluteToLineCol(n.Range.End+1)
                    | _ -> failwith "Unexpected node in pivot call chain" 

                  h?li ["class" => if selected then "selected" else ""] [ 
                    h?a ["click" =!> trigger (Select(identRange)) ] [
                      text (transformName sec.Transformation) 
                    ]
                  ]
                yield h?li ["class" => if state.Menus = AddDropdownOpen then "add selected" else "add"] [ 
                  h?a ["click" =!> trigger OpenAddDropdown ] [
                    h?i ["class" => "fa fa-plus"] [] 
                  ]
                ]
              ]
              h?div ["class" => "add-menu"] [
                let clickHandler tfs = "click" =!> trigger (AddTransform(tfs))
                if state.Menus = AddDropdownOpen then
                  yield h?ul [] [
                    h?li [] [ h?a [ clickHandler(Pivot.DropColumns []) ] [ text "drop columns"] ]
                    h?li [] [ h?a [ clickHandler(Pivot.FilterBy []) ] [ text "filter by"] ]
                    h?li [] [ h?a [ clickHandler(Pivot.GetSeries("!", "!")) ] [ text "get series"] ]
                    h?li [] [ h?a [ clickHandler(Pivot.GroupBy([], [])) ] [ text "group by"] ]
                    h?li [] [ h?a [ clickHandler(Pivot.Paging []) ] [ text "paging"] ]
                    h?li [] [ h?a [ clickHandler(Pivot.SortBy []) ] [ text "sort by"] ]
                  ]
              ]
              h?div ["class" => "preview-body"] [
                yield h.delayed preview
              ] 
            ]
          let endLine, _ = state.Mapper.AbsoluteToLineCol(body.Range.End)
          Some(endLine, dom)

let createPivotPreview updateZones (ed:monaco.editor.ICodeEditor) = 
  let pivotEvent = new Event<PivotEditorAction>()

  let mutable pivotState = 
    { Selection = None
      Mapper = Monaco.LocationMapper("")
      Code = ""
      Globals = []
      Location = 0
      Body = None
      Program = { Body = Ast.node { Start = 0; End = 0 } [] }
      Menus = Hidden }

  pivotEvent.Publish.Add(fun evt ->
    try
      Log.trace("live", "Updating state %O with event %O", pivotState, evt)
      pivotState <- updatePivotState pivotState evt 
      match pivotState.Selection with
      | Some rng ->
          ed.setSelection(rng)
          ed.focus()
          pivotState <- { pivotState with Selection = None }
      | _ -> ()
      updateZones (renderPivot pivotEvent.Trigger pivotState)
    with e ->
      Log.exn("live", "Error when updating state %O with event %O", pivotState, evt) )

  async { let! glob = globalTypes |> Async.AwaitFuture 
          pivotEvent.Trigger(InitializeGlobals glob) } |> Async.StartImmediate

  pivotEvent.Trigger

// ------------------------------------------------------------------------------------------------
// Zones infra
// ------------------------------------------------------------------------------------------------

type PreviewService(checker:CheckingService, ed:monaco.editor.ICodeEditor) =
  let mutable currentZone : option<_* monaco.editor.IViewZone *_> = None

  let removeZone () =
    match currentZone with 
    | Some(id, _, _) -> ed.changeViewZones(fun accessor -> accessor.removeZone(id))
    | None -> ()
    currentZone <- None

  let createAndAddZone endLine =
    let mutable zoneId = -1.
    let zone = JsInterop.createEmpty<monaco.editor.IViewZone>
    
    let node = document.createElement_div()
    let wrapper = document.createElement_div()
    node.appendChild(wrapper) |> ignore
    ed.changeViewZones(fun accessor ->  
      match currentZone with Some(id, _, _) -> accessor.removeZone(id) | _ -> ()
      zone.afterLineNumber <- endLine
      zone.heightInPx <- Some 300.0
      zone.domNode <- node
      zoneId <- accessor.addZone(zone) 
      currentZone <- Some (zoneId, zone, wrapper) )

  let updateZones dom =
    match dom with 
    | None -> removeZone ()
    | Some(line, dom) ->
        if currentZone.IsNone then
          createAndAddZone 0.0
        Log.trace("live", "Render %O to zone %O", dom, currentZone)
        match currentZone with
        | Some(id, zone, wrapper) -> 
            if zone.afterLineNumber <> float line then
              zone.afterLineNumber <- float line
              ed.changeViewZones(fun accessor ->
                accessor.layoutZone(id)
              )
            dom |> renderTo wrapper
        | _ -> () // Shouldn't happen because we created the zone above 

  let trigger = createPivotPreview updateZones ed    
      
  let mutable lastCode = ""
  let mutable lastMapper = Monaco.LocationMapper("")
  do
    ed.onDidChangeCursorPosition(fun ce -> 
      async {
        let code = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
        if code <> lastCode then
          lastCode <- code
          lastMapper <- Monaco.LocationMapper(code)
          let loc = lastMapper.LineColToAbsolute(int ce.position.lineNumber, int ce.position.column)
          let! _, _, program = checker.TypeCheck(code)
          trigger (UpdateSource(code, loc, program, lastMapper)) 
        else 
          let loc = lastMapper.LineColToAbsolute(int ce.position.lineNumber, int ce.position.column)
          trigger (UpdateLocation(loc))   } |> Async.StartImmediate ) |> ignore
    ()

// ------------------------------------------------------------------------------------------------
// Putting everything togeter
// ------------------------------------------------------------------------------------------------


[<Emit("setRunner($0, $1)")>]
let setRunner (article:string) (f:unit -> unit) = failwith "JS"

[<Emit("shareSnippet($0, $1)")>]
let shareSnippet (snippet:string) (compiled:string) = failwith "JS"

[<Emit("cannotShareSnippet()")>]
let cannotShareSnippet () = failwith "JS"

let callShowMethod outId cmd = async {
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
    TheGamma.TypePovidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
    TypePovidersRuntime.trimLeft |> ignore
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
    PreviewService(checkingService, ed) |> ignore
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
