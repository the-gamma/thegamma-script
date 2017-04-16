module TheGamma.Monaco

open Fable.Core
open Fable.Import.monaco
open Fable.Import.Browser

open TheGamma.Common
open TheGamma.Services
open TheGamma.TypeChecker

[<Emit("var i = 1; eval('monaco$' + i + ' = monaco'); monaco = monaco;")>]
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

let createCompletionProvider (getService:string -> CheckingService) = 
  { new languages.CompletionItemProvider with 
      member this.triggerCharacters = Some(ResizeArray [ "." ])
      member this.provideCompletionItems(model, position, token) =           
        async {      
          try    
            let svc = getService (model.uri.toString())

            let input = model.getValue(editor.EndOfLinePreference.LF, false)
            Log.event("editor", "completions", "", JsInterop.createObj ["source", box input; "position", box position])

            let conv = LocationMapper(input)
            let loc = conv.LineColToAbsolute(int position.lineNumber, int position.column)
            
            let! _, ents, _ = svc.TypeCheck(input)
            let optMembers = 
              ents.Entities |> Seq.tryPick (fun (rng, ent) ->
                match ent.Kind with 
                | EntityKind.NamedMember(_, { Type = Some t }) when loc >= rng.Start && loc <= rng.End + 1 -> 
                    Log.trace("completions", "Antecedant at current location: %O", t)
                    match t with
                    | Type.Object obj -> Some(rng, obj.Members)
                    | _ -> None
                | EntityKind.NamedMember(n, { Type = Some t })  ->
                    Log.trace("completions", "Ignoring '%s' at location %s-%s (current=%s)", n.Name, rng.Start, rng.End, loc)
                    None
                | _ -> None)

            let convertRange (rng:TheGamma.Range) = 
              let sl, sc = conv.AbsoluteToLineCol(rng.Start)
              let el, ec = conv.AbsoluteToLineCol(rng.End)
              let res = JsInterop.createEmpty<IRange>
              res.startColumn <- float sc
              res.startLineNumber <- float sl
              res.endColumn <- float ec + 1.0
              res.endLineNumber <- float el
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
                      ci.insertText <- Some(Ast.escapeIdent n)
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
                      eo.text <- Ast.escapeIdent n
                      eo.range <- nameRange
                      ci.textEdit <- Some eo
                      ci ] 
                Log.trace("completions", "returning %O", Array.ofSeq completion)
                return ResizeArray(completion)
            with e ->
              Log.exn("completions", "completions failed %O", e)
              return ResizeArray() } |> Async.StartAsPromise |> Fable.Core.U4.Case2

      member this.resolveCompletionItem(item, token) = Fable.Core.U2.Case1 item }

let createdEditors = System.Collections.Generic.Dictionary<string, CheckingService>()
let getService uri = createdEditors.[uri]

let setupMonacoServices () = 
  let lang = JsInterop.createEmpty<languages.ILanguageExtensionPoint>
  lang.id <- "thegamma"
  languages.Globals.register(lang)
  languages.Globals.setTokensProvider("thegamma", tokensProvider) |> ignore
  languages.Globals.registerCompletionItemProvider("thegamma", createCompletionProvider getService) |> ignore

let createMonacoEditor id code svc customize = 
  if createdEditors.Count = 0 then setupMonacoServices ()
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
  let ed = editor.Globals.create(document.getElementById(id), options, services)
  createdEditors.Add(ed.getModel().uri.toString(), svc)
  ed


