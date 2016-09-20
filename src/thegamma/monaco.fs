module TheGamma.Monaco

open Fable.Core
open Fable.Import.monaco
open Fable.Import.Browser

open TheGamma.Common
open TheGamma.Services
open TheGamma.TypeChecker

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

        let tokenized, _ = Tokenizer.tokenize line
        for t in tokenized do
          let tok = JsInterop.createEmpty<languages.IToken>
          tok.startIndex <- float t.Range.Start
          tok.scopes <- Fable.Core.U2.Case1 (getColorClass t.Token)
          tokens.tokens.Add(tok)

        tokens
      member this.getInitialState() = noState }

let needsEscaping (s:string) = 
  (s.[0] >= '0' && s.[0] <= '9') ||
  (s.ToCharArray() |> Array.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')) ))

let escapeIdent s = 
  if needsEscaping s then "'" + s + "'" else s

let createCompletionProvider (getService:string -> CheckingService) = 
  { new languages.CompletionItemProvider with 
      member this.triggerCharacters = Some(ResizeArray [ "." ])
      member this.provideCompletionItems(model, position, token) =           
        async {      
          try    
            let svc = getService (model.uri.toString())

            let input = model.getValue(editor.EndOfLinePreference.LF, false)
            Log.event("editor", "completions", "", JsInterop.createObj ["source", box input; "position", box position])

            let lengths = input.Split('\n') |> Array.map (fun s -> s.Length)
            let loc = 
              int position.column - 1 +
                List.fold (+) 0 [ for i in 1 .. int position.lineNumber-1 -> lengths.[i-1] + 1 ]
            
            let! _, ents, _ = svc.TypeCheck(input)
            let optMembers = 
              ents |> Seq.tryPick (fun (rng, ent) ->
                match ent.Kind with 
                | EntityKind.NamedMember(_, { Type = Some t }) when loc >= rng.Start && loc <= rng.End + 1 -> 
                    Log.trace("completions", "Antecedant at current location: %O", t)
                    match TypeChecker.reduceType t with
                    | Type.Object { Members = mems } -> Some(rng, mems)
                    | _ -> None
                | _ -> None)

            let convertRange (rng:TheGamma.Range) = 
              let s = CodeGenerator.offsetToLocation 1 rng.Start (List.ofArray lengths)
              let e = CodeGenerator.offsetToLocation 1 rng.End (List.ofArray lengths)
              let res = JsInterop.createEmpty<IRange>
              res.startColumn <- float s.column+1.0
              res.startLineNumber <- float s.line
              res.endColumn <- float e.column+2.0
              res.endLineNumber <- float e.line
              res

            match optMembers with 
            | None -> 
                Log.trace("completions", "no members at %s", loc)
                return ResizeArray []
            | Some (nameRange, members) -> 
                let nameRange = convertRange nameRange
                Log.trace("completions", "providing %s members at %O", members.Length, nameRange)
                let completion =
                  [ for m in members ->
                      let ci = JsInterop.createEmpty<languages.CompletionItem>
                      let n, k =
                        match m with 
                        | Member.Method(name=n) -> n, languages.CompletionItemKind.Method
                        | Member.Property(name=n) -> n, languages.CompletionItemKind.Property
                      ci.kind <- k
                      ci.label <- n
                      ci.insertText <- Some(escapeIdent n)
                      ci.filterText <- Some(n)
                      match m with
                      | Member.Method(arguments=args) -> 
                          let acc, l = 
                            [ for n, opt, t in args -> (if opt then "?" else "") + n ] 
                            |> Seq.fold (fun (acc, l:string) s ->
                                if l.Length > 100 then (l::acc, s)
                                else (acc, if l = "" then s else l+","+s)) ([], "")
                          let args = l::acc |> List.rev |> String.concat ",\n"
                          ci.documentation <- Some("(" + args + ")")
                      | _ -> ()

                      let eo = JsInterop.createEmpty<editor.ISingleEditOperation>
                      eo.text <- if needsEscaping n then "'" + n + "'" else n
                      eo.range <- nameRange
                      ci.textEdit <- Some eo
                      ci ] 
                Log.trace("completions", "returning %O", Array.ofSeq completion)
                return ResizeArray(completion)
            with e ->
              Log.exn("completions", "completions failed %O", e)
              return ResizeArray() } |> Async.StartAsPromise |> Fable.Core.U4.Case2

      member this.resolveCompletionItem(item, token) = Fable.Core.U2.Case1 item }

let setupMonacoServices (getService : string -> CheckingService) =
  let lang = JsInterop.createEmpty<languages.ILanguageExtensionPoint>
  lang.id <- "thegamma"
  languages.Globals.setTokensProvider("thegamma", tokensProvider) |> ignore
  languages.Globals.registerCompletionItemProvider("thegamma", createCompletionProvider getService) |> ignore
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


