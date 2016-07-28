#r "../paket-files/github.com/fsprojects/Fable/build/fable/bin/Fable.Core.dll"
#r "libraries/bin/Debug/libraries.dll"
#r "thegamma/bin/Debug/thegamma.dll"
#r "bindings/bin/Debug/bindings.dll"
#r "gui/bin/Debug/gui.dll"
open Fable.Core.Extensions
open Fable.Import
open Fable.Import.Browser
open Fable.Import.monaco
open Fable.Extensions
module FsOption = Microsoft.FSharp.Core.Option

open TheGamma
open TheGamma.Html
open TheGamma.AstOperations
open TheGamma.Babel
module F = Fable.Core.Operators

type BabelOptions = 
  { presets : string[] }

type BabelResult = 
  { code : string }
type Babel =
  abstract transformFromAst : obj * string * BabelOptions -> BabelResult

[<Emit("eval($0)")>]
let evalCode (s:string) : unit = ()

[<Emit("Babel")>]
let babel : Babel = Unchecked.defaultof<_> 

[<Emit("_monaco = monaco;")>]
let hack : unit = ()
hack


// Global provided types
type ProvidedTypes = 
  { LookupNamed : string -> Type list -> Type
    Globals : Map<string, Expression * Type> }

let types = async {
  let mutable named = Map.empty
  let lookupNamed n tyargs = 
    match named.TryFind(n) with
    | Some r -> r
    | None -> 
        Browser.console.log("Could not find named type '%s'", n)
        failwith (sprintf "Could not find named type '%s'" n)

  let restTys = 
    [ TypePoviders.RestProvider.provideRestType lookupNamed "olympics1" "http://127.0.0.1:10042/olympics" ""
      TypePoviders.RestProvider.provideRestType lookupNamed "olympics" "http://127.0.0.1:10042/pivot" "source=http://127.0.0.1:10042/olympics"
      TypePoviders.RestProvider.provideRestType lookupNamed "adventure" "http://127.0.0.1:10042/adventure" ""
      TypePoviders.RestProvider.provideRestType lookupNamed "world" "http://127.0.0.1:10042/worldbank" ""
      
      // TODO: some more types 
      TypePoviders.NamedType("value", Type.Any)
      TypePoviders.NamedType("seq", Type.Any) 
      ]
  let! fsTys = TypePoviders.FSharpProvider.provideFSharpTypes lookupNamed "out/fsprovider/libraries.json"     
  let allTys = restTys @ fsTys

  named <- 
    allTys 
    |> Seq.choose (function TypePoviders.NamedType(s, t) -> Some(s, t) | _ -> None)
    |> Map.ofSeq

  let globals = 
    allTys 
    |> List.choose (function TypePoviders.GlobalValue(s, e, t) -> Some(s, (e, t)) | _ -> None)
    |> Map.ofSeq
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsFuture 
    

let mapNameRanges f (n:Name) = 
  { n  with Range = f n.Range }

let rec mapExprRanges f expr = 
  match expr.Expr with  
  | ExprLeaf -> { expr with Range = f expr.Range }
  | ExprNode(es, ns) -> 
      { Expr = rebuildExprNode expr.Expr (List.map (mapExprRanges f) es) (List.map (mapNameRanges f) ns)
        Range = f expr.Range; Type = expr.Type }

let rec mapCmdRanges f cmd = 
  match cmd.Command with
  | CommandKind.Expr e -> { Command = CommandKind.Expr (mapExprRanges f e); Range = cmd.Range }
  | CommandKind.Let(n, e) -> { Command = CommandKind.Let(mapNameRanges f n, mapExprRanges f e); Range = cmd.Range }

let tokenize (input:string) = 
  let input = input.Replace("\r\n", "\n")
  let (Parsec.Parser p) = Tokenizer.tokens
  match p (0, List.ofSeq input) with
  | Some((offs, rest), errors, tokens) ->
      let errors = 
        if List.isEmpty rest then errors
        else 
          let rest = System.String(Array.ofList rest)
          { Number = 11; Range = { Start = offs; End = offs + rest.Length }
            Message = sprintf "Tokenizer stopped: %s" rest }::errors 
      errors, tokens
  | None ->
      [ { Number = 11; Range = { Start = 0; End = input.Length }
          Message = sprintf "Tokenizer did not recognize input: %s" input } ], []

let parse (input:string) = 
  let errs1, tokens = tokenize input
  let (Parsec.Parser p) = Parser.program

  let rangeLookup = tokens |> List.map (fun tok -> tok.Range) |> Array.ofSeq

  let tokToChar rng =
    let safe start n = 
      if n >= rangeLookup.Length then rangeLookup.[rangeLookup.Length-1].End
      elif n < 0 then 0
      elif start then rangeLookup.[n].Start
      else rangeLookup.[n].End
    let rng = 
      { Start = safe true rng.Start
        End = safe false (rng.End-1) }
    if rng.End < rng.Start then { rng with End = rng.Start }
    else rng

  match p (0, tokens) with
  | Some((offs, rest), errs2, prog) ->
      let errs2 = errs2 |> List.map (fun e -> { e with Range = tokToChar e.Range })
      let errors = 
        if List.isEmpty rest then errs1 @ errs2
        else
          { Number = 21; Range = tokToChar { Start = offs; End = offs + List.length rest }
            Message = sprintf "Parser stopped: %A" rest } :: errs1 @ errs2
      errors, { Range = prog.Range; Body = prog.Body |> List.map (mapCmdRanges tokToChar) }
  | _ ->
    { Number = 21; Range = tokToChar { Start = 0; End = List.length tokens }
      Message = sprintf "Parser stopped: %A" tokens } :: errs1,
    { Range = tokToChar { Start = 0; End = List.length tokens }
      Body = [] }
          
let typeCheck input = async {
  let! ptys = types |> Async.AwaitFuture
  let errs1, untyped = parse input
  let! checkd, ctx = TypeChecker.typeCheckProgram { Variables = Map.map (fun _ (_, t) -> t) ptys.Globals } { Errors = [] } untyped
  return errs1 @ ctx.Errors, checkd }


let renderErrors errors = 
  h?div ["class" => "error"] 
    [ for (e:Error) in errors -> 
        h?div [] [
          text (sprintf "%d:%d" e.Range.Start e.Range.End); 
          text "error "; text (string e.Number); text ": "; text (e.Message)] ]

let reportErrors errors = 
  renderErrors errors |> renderTo (document.getElementById("errors"))

let services = F.createEmpty<editor.IEditorOverrideServices>

let lang = F.createEmpty<languages.ILanguageExtensionPoint>
lang.id <- "thegamma"

let noState = 
  { new languages.IState with
      member this.clone() = this
      member this.equals(other) = true }

let toks = 
  { new monaco.languages.TokensProvider with
      member this.tokenize(line, state) =
        let tokens = F.createEmpty<languages.ILineTokens>
        tokens.endState <- noState
        tokens.tokens <- ResizeArray()

        let _, tokenized = tokenize line
        for t in tokenized do
          let tok = F.createEmpty<languages.IToken>
          tok.startIndex <- float t.Range.Start
          tok.scopes <- Fable.Core.U2.Case1 (match t.Token with TokenKind.QIdent _ | TokenKind.Ident _ -> "" | TokenKind.Dot _ -> "operator" | TokenKind.Let | TokenKind.Boolean _ -> "keyword" | TokenKind.Number _ -> "number" | _ -> "")
          tokens.tokens.Add(tok)

        tokens
      member this.getInitialState() = noState }

monaco.languages.Globals.setTokensProvider("thegamma", toks) |> ignore


let needsEscaping (s:string) = 
  (s.[0] >= '0' && s.[0] <= '9') ||
  (s.ToCharArray() |> Array.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) ))

let compr = 
  { new languages.CompletionItemProvider with 
      member this.triggerCharacters = Some(ResizeArray [ "." ])
      member this.provideCompletionItems(model, position, token) =           
        async {          
          try
            let input = model.getValue(editor.EndOfLinePreference.LF, false)
            let lines = input.Split('\n')
            let! errs, ty = typeCheck input
            let! info = TypeChecker.collectProgramInfo { Completions = []; Source = input } ty
            
            let absPosition = 
              int position.column - 1 +
                List.fold (+) 0 [ for i in 1 .. int position.lineNumber-1 -> lines.[i-1].Length + 1 ]
  
            let optMembers = 
              info.Completions 
              |> List.filter (fun (rng, _, _) -> absPosition >= rng.Start && absPosition <= rng.End)
              |> List.sortBy (fun (rng, _, _) -> -rng.Start)
              |> List.tryHead

            let log = 
              [| for r, _, c in info.Completions -> 
                  let opts = c |> Seq.truncate 3 |> Seq.map (fun m -> m.Name) |> String.concat ", "
                  sprintf "%d - %d: %s..." r.Start r.End opts |]
            Log.trace("completions", "requested at: %O", position)
            Log.trace("completions", "available: %O", log)

            let lengths = lines |> Array.map String.length |> List.ofSeq

            let convertRange (rng:Range) = 
              let s = CodeGenerator.offsetToLocation 1 rng.Start lengths
              let e = CodeGenerator.offsetToLocation 1 rng.End lengths
              let res = F.createEmpty<IRange>
              res.startColumn <- float s.column
              res.startLineNumber <- float s.line
              res.endColumn <- float e.column
              res.endLineNumber <- float e.line
              res

            match optMembers with 
            | None -> 
                Log.trace("completions", "no members at %s", absPosition)
                return ResizeArray []
            | Some (_, nameRange, members) -> 
                let nameRange = convertRange nameRange
                let completion =
                  [ for m in members ->
                      let ci = F.createEmpty<languages.CompletionItem>
                      let n, k =
                        match m with 
                        | Member.Method(name=n) -> n, languages.CompletionItemKind.Method
                        | Member.Property(name=n) -> n, languages.CompletionItemKind.Property
                      ci.kind <- k
                      ci.label <- n
                      ci.insertText <- Some(if needsEscaping n then "'" + n + "'" else n)
                      ci.filterText <- Some(n)
                      let eo = F.createEmpty<editor.ISingleEditOperation>
                      eo.text <- if needsEscaping n then "'" + n + "'" else n
                      eo.range <- nameRange
                      //ci.textEdit <- Some eo
                      ci ] 
                Log.trace("completions", "returning %O", Array.ofSeq completion)
                return ResizeArray(completion)
            with e ->
              Log.exn("completions", "type checking failed %O", e)
              return ResizeArray [] } |> Async.StartAsPromise |> Fable.Core.U4.Case2

      member this.resolveCompletionItem(item, token) = Fable.Core.U2.Case1 item }

monaco.languages.Globals.registerCompletionItemProvider("thegamma", compr)
|> ignore

monaco.languages.Globals.register(lang)


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

let options = F.createEmpty<editor.IEditorConstructionOptions>
options.value <- Some sample
options.language <- Some "thegamma"

let ed = monaco.editor.Globals.create(Browser.document.getElementById("container"), options, services)
()

let escape s = 
  if needsEscaping s then "'" + s + "'" else s

let replace (rng:Range) newValue (text:string) = 
  text.Substring(0, rng.Start) + newValue + text.Substring(rng.End)

let replaceNameWithValue (text:string) (n:Name) el e =
  let newValue = escape (unbox<HTMLSelectElement> el).value
  let newText = replace n.Range newValue text
  ed.getModel().setValue(newText)

/// Replace the second string first, assuming it is later in the text
let replaceTwoNamesWithValues (text:string) (n1:Name, n2:Name) (s1, s2) =
  let newText = replace n1.Range (escape s1) (replace n2.Range (escape s2) text) 
  ed.getModel().setValue(newText)

let removeRangeWithPrecendingDot (text:string) (rng:Range) = 
  // Once we have comments, we need to skip over them too
  let mutable start = rng.Start
  while start > 0 && text.[start] <> '.'  do start <- start - 1
  let newText = text.Substring(0, start) + text.Substring(rng.End)
  ed.getModel().setValue(newText)

let insertDotTextAfter (origText:string) (rng:Range) ins =
  let newText = origText.Substring(0, rng.End) + "." + escape ins + origText.Substring(rng.End)
  ed.getModel().setValue(newText)

let renderEditor origText = function
  | Editors.SingleChoice(n, ms) ->
      h?div [] [
        h?h2 [] [text "choose one"]
        h?select 
          [ "change" =!> replaceNameWithValue origText n ] 
          [ for (Editors.Property(name, _, _)) in ms ->
              let sel = if name = n.Name then ["selected" => "selected"] else []
              h?option sel [ text name ] ]
      ]
  | Editors.CreateList(ca, ns, ms) ->
      h?div [] [
        h?h2 [] [text "create list"]
        h?ul [] [
          for n in ns -> 
            h?li [] [ 
              text n.Name 
              text " "
              h?a ["click" =!> fun el e -> removeRangeWithPrecendingDot origText n.Range ] [ text "X" ]
            ]
        ]
        h?select 
          [ "data-placeholder" => "Add another item..."
            "change" =!> fun el e -> 
              let sel = (el :?> HTMLSelectElement).value
              let last = if ns.Length = 0 then ca else ns.[ns.Length - 1]
              insertDotTextAfter origText last.Range sel
          ] 
          [ yield h?option [] []
            for (Editors.Property(name, _, _)) in ms ->
              h?option [] [ text name ]
          ]
      ]
  | Editors.NestedChoice(n1, n2, props) ->
      h.part (n1.Name, n2.Name) (fun el (name1, name2) update ->
        let selected = 
          props 
          |> Array.tryFind (fun (Editors.Property(name, _, _), nested) -> name = name1) 
          |> FsOption.map snd
        let nested = defaultArg selected [||]

        h?div [] [
          h?h2 [] [text "nested choice"]
          h?select 
            [ "change" =!> fun el e -> update ((unbox<HTMLSelectElement> el).value, "") ] 
            [ for (Editors.Property(name, _, _), nested) in props ->
                let sel = if name = name1 then ["selected" => "selected"] else []
                h?option sel [ text name ] ]
          h?select 
            [ "data-placeholder" => "Choose an item..." 
              "change" =!> fun el e -> 
                  let name2 = (unbox<HTMLSelectElement> el).value
                  replaceTwoNamesWithValues origText (n1, n2) (name1, name2) ] 
            [ if name2 = "" then yield h?option [] []
              for Editors.Property(name, _, _) in nested ->
                let sel = if name = name2 then ["selected" => "selected"] else []
                h?option sel [ text name ] ]
        ] |> renderTo el
      )


type EditorWorker() = 
  let update text = async {
    Log.trace("editors", "type checking")
    let! errs, prg = typeCheck text 
    reportErrors errs

    Log.trace("editors", "collecting")
    let! eds = Async.collect Editors.collectCmdEditors prg.Body 
    let eds = eds |> List.mapi (fun i v -> i, v)
    let filteredEds = 
      eds 
      |> List.filter (fun (i, ed1) ->
          eds |> List.exists (fun (j, ed2) -> j <> i && Ranges.subRange ed1.Range ed2.Range) |> not)
      |> List.map snd
            
    Log.trace("editors", "rendering %s out of %s", eds.Length, filteredEds.Length)
    h?div [] (List.map (renderEditor text) filteredEds)
    |> renderTo (document.getElementById("editor")) }

  let mutable lastText = None
  let mutable resume = ignore

  let textChanged = Async.FromContinuations(fun (cont, _, _) ->
    let rec loop () =
      match lastText with
      | Some text ->
          resume <- ignore
          lastText <- None
          cont(text)
      | None -> resume <- loop
    loop ())

  let worker = async { 
    while true do
      try 
        let! text = textChanged
        do! update text          
      with e -> 
        Log.exn("editors", "update failed: %O", e)
  }

  do worker |> Async.StartImmediate

  member x.Update(text) =
    Log.trace("editors", "text updated")
    lastText <- Some text
    resume()


let edWorker = EditorWorker()

ed.getModel().onDidChangeContent(fun ch ->
  let text = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
  edWorker.Update(text)  
) |> ignore
  

document.getElementById("run").onclick <- fun e ->
    
  let text = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
  async {
    try
      let! ptys = types |> Async.AwaitFuture
      let! errs, prog = typeCheck text 
      reportErrors errs
        
      let ctx = 
        { CodeGenerator.LineLengths = [ for l in text.Split('\n') -> l.Length ] 
          CodeGenerator.Globals = ptys.Globals |> Map.map (fun _ (e, _) -> e) }

      let! res = CodeGenerator.compileProgram ctx prog

      let code = babel.transformFromAst(Serializer.serializeProgram res, text, { presets = [| "es2015" |] })

      //Browser.window.alert(code.code)
      Browser.console.log(code)

      // Get fable to reference everything
      let s = TheGamma.Series.series<int, int>.create(async { return [||] }, "", "", "") 
      TheGamma.TypePovidersRuntime.RuntimeContext("lol", "", "troll") |> ignore
      TypePovidersRuntime.trimLeft |> ignore
      TheGamma.GoogleCharts.chart.bar |> ignore
      TheGamma.table<int, int>.create(s) |> ignore

      evalCode code.code

      // let lengths = 

    with e ->
      Browser.console.log("*** Type checking failed ***")
      Browser.console.log(e) } |> Async.StartImmediate

  box()
