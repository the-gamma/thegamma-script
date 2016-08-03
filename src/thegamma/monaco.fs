module TheGamma.Monaco

open Fable.Core
open Fable.Import.monaco
open Fable.Import.Browser
open TheGamma.TypeChecker
open Fable.Extensions

[<Emit("_monaco = monaco;")>]
let hack : unit = ()
hack

let noState = 
  { new languages.IState with
      member this.clone() = this
      member this.equals(other) = true }

let getColorClass = function
  | TokenKind.String _ -> "string" 
  | TokenKind.QIdent _ | TokenKind.Ident _ -> "ident" 
  | TokenKind.Dot _ -> "operator" 
  | TokenKind.By | TokenKind.To | TokenKind.Let 
  | TokenKind.Boolean _ | TokenKind.Fun | TokenKind.Arrow -> "keyword" 
  | TokenKind.Number _ -> "number" 
  | _ -> ""

let tokensProvider = 
  { new languages.TokensProvider with
      member this.tokenize(line, state) =
        let tokens = JsInterop.createEmpty<languages.ILineTokens>
        tokens.endState <- noState
        tokens.tokens <- ResizeArray()

        let _, tokenized = tokenize line
        for t in tokenized do
          let tok = JsInterop.createEmpty<languages.IToken>
          tok.startIndex <- float t.Range.Start
          tok.scopes <- Fable.Core.U2.Case1 (getColorClass t.Token)
          tokens.tokens.Add(tok)

        tokens
      member this.getInitialState() = noState }

let createCompletionProvider globals = 
  { new languages.CompletionItemProvider with 
      member this.triggerCharacters = Some(ResizeArray [ "." ])
      member this.provideCompletionItems(model, position, token) =           
        async {          
          try
            let input = model.getValue(editor.EndOfLinePreference.LF, false)
            let lines = input.Split('\n')
            let! globals = Async.AwaitFuture globals
            let! errs, ty = typeCheck globals input
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

            let convertRange rng = 
              let s = CodeGenerator.offsetToLocation 1 rng.Start lengths
              let e = CodeGenerator.offsetToLocation 1 rng.End lengths
              let res = JsInterop.createEmpty<IRange>
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
                      let ci = JsInterop.createEmpty<languages.CompletionItem>
                      let n, k =
                        match m with 
                        | Member.Method(name=n) -> n, languages.CompletionItemKind.Method
                        | Member.Property(name=n) -> n, languages.CompletionItemKind.Property
                      ci.kind <- k
                      ci.label <- n
                      ci.insertText <- Some(if needsEscaping n then "'" + n + "'" else n)
                      ci.filterText <- Some(n)
                      let eo = JsInterop.createEmpty<editor.ISingleEditOperation>
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

let setupMonacoServices globals =
  let lang = JsInterop.createEmpty<languages.ILanguageExtensionPoint>
  lang.id <- "thegamma"
  languages.Globals.setTokensProvider("thegamma", tokensProvider) |> ignore
  languages.Globals.registerCompletionItemProvider("thegamma", createCompletionProvider globals) |> ignore
  languages.Globals.register(lang)

let createMonacoEditor id code customize = 
  let services = JsInterop.createEmpty<editor.IEditorOverrideServices>
  let options = JsInterop.createEmpty<editor.IEditorConstructionOptions>
  let scroll = JsInterop.createEmpty<editor.IEditorScrollbarOptions>
  scroll.vertical <- Some "none"
  scroll.horizontal <- Some "auto"
  options.scrollbar <- Some scroll
  options.value <- Some code
  options.language <- Some "thegamma"
  options.lineNumbersMinChars <- Some 3.0
  options.contextmenu <- Some false
  options.scrollBeyondLastLine <- Some false
  options.overviewRulerLanes <- Some 0.0
  customize options
  editor.Globals.create(document.getElementById(id), options, services)



