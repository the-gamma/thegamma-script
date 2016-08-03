#r "../paket-files/github.com/fsprojects/Fable/build/fable/bin/Fable.Core.dll"
#r "libraries/bin/Debug/libraries.dll"
#r "thegamma/bin/Debug/thegamma.dll"
#r "bindings/bin/Debug/bindings.dll"
#r "gui/bin/Debug/gui.dll"
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

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

// let services = "http://127.0.0.1:10042/"
let services = "http://thegamma-services.azurewebsites.net/"

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
        TypeChecker.substituteTypes (Map.ofList (List.zip tya tyargs)) r
    | None -> 
        Log.error("Could not find named type '%s'", n)
        failwith (sprintf "Could not find named type '%s'" n)

  let restTys = 
    [ TypePoviders.RestProvider.provideRestType lookupNamed 
        "olympics1" (services + "olympics") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "olympics" (services + "pivot") ("source=" + services + "olympics")
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      // TODO: some more types 
      TypePoviders.NamedType("value", ["a"], Type.Any)
      TypePoviders.NamedType("seq", ["a"], Type.Any) 
      TypePoviders.NamedType("async", ["a"], Type.Any) ]

  let! fsTys = TypePoviders.FSharpProvider.provideFSharpTypes lookupNamed "out/fsprovider/libraries.json"     
  let allTys = restTys @ fsTys

  named <- 
    allTys 
    |> Seq.choose (function TypePoviders.NamedType(s, tya, t) -> Some(s, (t, tya)) | _ -> None)
    |> Map.ofSeq

  let globals = 
    allTys 
    |> List.choose (function TypePoviders.GlobalValue(s, e, t) -> Some(s, (e, t)) | _ -> None)
    |> Map.ofSeq
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsFuture 

let globalTypes = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> Map.map (fun _ (_, t) -> t) } |> Async.StartAsFuture

let globalExprs = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> Map.map (fun _ (e, _) -> e) } |> Async.StartAsFuture

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
            let rng = { Start = e.Range.End; End = e.Range.End }
            let outExpr = { Expr = ExprKind.String(outId); Range = rng; Type = Type.Primitive("string") }
            let args = [{ Argument.Name = None; Argument.Value = outExpr }]
            let newE = { e with Expr = ExprKind.Call(e, { Name = "show"; Range = rng }, args) }
            return { cmd with Command = CommandKind.Expr(newE) }
          else 
            return cmd
      | _ -> return cmd
  | _ -> return cmd }

let renderErrors el errors = 
  h?div ["class" => "error"] 
    [ for (e:Error) in errors -> 
        h?div [] [
          text (sprintf "%d:%d" e.Range.Start e.Range.End); 
          text "error "; text (string e.Number); text ": "; text (e.Message)] ]
  |> renderTo el

let setupEditor (parent:HTMLElement) =
  let source = (findChildElement (withClass "ia-source") parent).innerText.Trim()
  let outputId = (findChildElement (withClass "ia-output") parent).id
    
  let runBtn = findChildElement (withClass "ia-run") parent
  let showCodeBtn = findChildElement (withClass "ia-show-source") parent
  let showOptionsBtn = tryFindChildElement (withClass "ia-show-options") parent
  
  let editorEl = findChildElement (withClass "ia-editor") parent
  let monacoEl = findChildElement (withClass "ia-monaco") parent
  let errorsEl = findChildElement (withClass "ia-errors") parent
  let optionsEl = findChildElement (withClass "ia-options") parent

  let checkingService = CheckingService(globalTypes)
  let editorService = EditorService(checkingService.TypeCheck, 2000)
  checkingService.ErrorsReported.Add (renderErrors errorsEl)

  let run text = async {
    let! prog = checkingService.TypeCheck(text)
    let! newBody = prog.Body |> Async.map (callShowMethod outputId)
    let prog = { prog with Body = newBody }
    return! CodeGenerator.compileAndRun globalExprs text prog }

  run source |> Async.StartImmediate

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

  let setText t = 
    ed.Value.getModel().setValue(t)
    if showOptionsBtn.IsSome then
      editorService.UpdateSource(t, true)
    run(t) |> Async.StartImmediate

  let mutable optionsVisible = false
  let mutable editorVisible = false

  showOptionsBtn |> FsOption.iter (fun btn -> 
    editorService.EditorsUpdated.Add (fun eds ->
      h?div ["class" => "ia-editor-panel"] (List.map (Editors.renderEditor checkingService.IsWellTyped setText (getText())) eds)
      |> renderTo optionsEl )
  
    btn.onclick <- fun _ ->
      optionsVisible <- not optionsVisible
      optionsEl.style.display <- if optionsVisible then "block" else "none"
      if optionsVisible then editorService.UpdateSource(source)
      box () )

  showCodeBtn.onclick <- fun _ ->
    editorVisible <- not editorVisible
    editorEl.style.display <- if editorVisible then "block" else "none"
    if editorVisible then ed.Force() |> ignore
    box ()
  
  runBtn.onclick <- fun e -> getText() |> run |> Async.StartImmediate |> box


Monaco.setupMonacoServices(globalTypes)

for el in findElements (withClass "ia-figure") document.body do
  setupEditor (el :?> HTMLElement)


(*
let sample1 = """let data =
  world
    .byCountry.China
    .'Climate Change'.'CO2 emissions (kt)'
    .take(10)

chart.show(chart.column(data))"""
let sample2 = """let phelps = 
  olympics.'by athlete'.'United States'  
    .'PHELPS, Michael'.then.data
  .'filter columns'
    .'drop Athlete'.'drop Sport'.'drop Gold'.'drop Silver'.'drop Bronze'
  .then.'get the data'

table.create(phelps).show()"""

let sample = """let data = 
  olympics.data
    .'group data'.'by Athlete'
      .'count all'.'sum Gold'.'sum Silver'.'sum Bronze'
      .'concatenate values of NOC'.then
    .'sort data'
      .'by Gold descending'.'and by Silver descending'
      .'and by Bronze descending'.then
    .paging
      .take(10)
    .'get the data'

table.create(data).show()"""

*)

