#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../libraries/bin/Debug/libraries.dll"
#r "../thegamma/bin/Debug/thegamma.dll"
#r "../bindings/bin/Debug/bindings.dll"
#r "../gui/bin/Debug/gui.dll"
open Fable.Core.Extensions
open Fable.Import
open Fable.Import.Browser
open Fable.Extensions
module FsOption = Microsoft.FSharp.Core.Option

open TheGamma
open TheGamma.Html
open TheGamma.Babel
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
    Globals : Map<string, Expression * Type> }
    
let types = async {
  let mutable named = Map.empty
  let lookupNamed n tyargs = 
    match named.TryFind(n) with
    | Some(r, tya) -> 
        if List.length tya <> List.length tyargs then 
          Log.error("Named type '%s' has mismatching length of type arguments", n)
          failwith (sprintf "Named type '%s' has mismatching length of type arguments" n)
        TypeChecker.applyTypes (Map.ofList (List.zip tya tyargs)) r
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
        [ "Games", "string"; "Year", "num";  "Sport", "string"; "Discipline", "string" 
          "Athlete", "string"; "Team", "string"; "Gender", "string"; "Event", "string" 
          "Medal", "string"; "Gold", "num"; "Silver", "num"; "Bronze", "num" ]
      
      TypeProviders.Pivot.providePivotType (services + "pdata/smlouvy") "smlouvy" lookupNamed
        [ "Uzavřeno", "string"; "Publikováno", "string"; "Hodnota", "num"
          "Chybí hodnota", "string"; "Subjekt", "string"; "Útvar", "string"
          "Schválil", "string"; "Předmět", "string"; "Odkaz", "string"
          "Platnost", "string"; "Příjemci", "string"; "Příjemci (IČO)", "string" ]            

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
    |> List.choose (function TypePoviders.GlobalValue(s, e, t) -> Some(s, (e, t)) | _ -> None)
    |> Map.ofSeq
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsFuture "types"

let globalTypes = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> Map.map (fun _ (_, t) -> t) } |> Async.StartAsFuture "global types"

let globalExprs = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> Map.map (fun _ (e, _) -> e) } |> Async.StartAsFuture "global exps"

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

open TheGamma.Services

[<Emit("setRunner($0, $1)")>]
let setRunner (article:string) (f:unit -> unit) = failwith "JS"

[<Emit("shareSnippet($0, $1)")>]
let shareSnippet (snippet:string) (compiled:string) = failwith "JS"

[<Emit("cannotShareSnippet()")>]
let cannotShareSnippet () = failwith "JS"

let callShowMethod outId cmd = async {
  match cmd.Command with
  | CommandKind.Expr(e) ->
      let! m = getObjectMembers e.Type
      match m with 
      | ObjectMembers.Members(members) ->
          let hasShow = members |> Array.exists (function 
            | Member.Method("show", [], [_, _, Type.Primitive "string"], _, _, _) -> true
            | _ -> false)
          if hasShow then
            let rng = { Range.Start = e.Range.End; End = e.Range.End }
            let outExpr = { Expr = ExprKind.String(outId); Range = rng; Type = Type.Primitive("string") }
            let args = [{ Argument.Name = None; Argument.Value = outExpr }]
            let newE = { e with Expr = ExprKind.Call(e, { Name = "show"; Range = rng }, args) }
            return { cmd with Command = CommandKind.Expr(newE) }
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
        let! _, prog = checkingService.TypeCheck(text)
        let! newBody = prog.Body |> Async.map (callShowMethod outputId)
        let prog = { prog with Body = newBody }
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
      editorService.UpdateSource(text) 
      resizeEditor text) |> ignore
      
    resizeEditor source
    ed )
  
  let getText() = 
    if not ed.IsValueCreated then source
    else ed.Value.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)

  let setText (edit:string) membr t = 
    Log.event("options", "set-text", article, JsInterop.createObj ["edit", box edit; "member", box membr ])
    ed.Value.getModel().setValue(t)
    if showOptionsBtn.IsSome then
      editorService.UpdateSource(t, true)
    run(t) |> Async.StartImmediate

  let mutable optionsVisible = false
  let mutable editorVisible = false

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
    if editorVisible then ed.Force() |> ignore
    
  showCodeBtn.onclick <- fun _ -> switchEditor(); box()
  if source.Contains("empty.create") then switchEditor()

  shareBtn.onclick <- fun e -> 
    let text = getText()
    Log.event("gui", "share", article, text)
    async { 
      let! ok, prog = checkingService.TypeCheck(text)
      let! newBody = prog.Body |> Async.map (callShowMethod "output-id-placeholder")
      let prog = { prog with Body = newBody }
      let! compiled = CodeGenerator.compileAndRun globalExprs text prog         
      if not ok then cannotShareSnippet()
      else shareSnippet text compiled } |> Async.StartImmediate
    box ()

  runBtn.onclick <- fun e -> 
    Log.event("gui", "run", article, "click")
    getText() |> run |> Async.StartImmediate |> box


Monaco.setupMonacoServices(globalTypes)

for el in findElements (withClass "ia-figure") document.body do
  setupEditor (el :?> HTMLElement)
