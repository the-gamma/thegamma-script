module TheGamma.Main

open Fable.Core
open Fable.Core.Extensions
open Fable.Import
open Fable.Import.Browser
open Fable.Import.monaco
module FsOption = Microsoft.FSharp.Core.Option

[<Emit("JSON.stringify($0)")>]
let jsonStringify json : string = failwith "JS Only"

[<Emit("JSON.parse($0)")>]
let jsonParse<'R> (str:string) : 'R = failwith "JS Only"

module Http =
  /// Send HTTP request asynchronously
  /// (does not handle errors properly)
  let Request(meth, url, data) =
    Async.FromContinuations(fun (cont, _, _) ->
      let xhr = XMLHttpRequest.Create()
      xhr.``open``(meth, url)
      xhr.onreadystatechange <- fun _ ->
        if xhr.readyState > 3. && xhr.status = 200. then
          cont(xhr.responseText)
        obj()
      xhr.send(defaultArg data "") )

[<Emit("_monaco = monaco")>]
let hack : unit = failwith "JS only"
hack


type Type = { kind:string }
type TypeNested = { kind:string (* = nested *); endpoint:string }
type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

type Member =
  { name : string
    returns : Type
    trace : string[] }

let load url = async {
  let! json = Http.Request("GET", url, None)
  let members = jsonParse<Member[]> json
  Browser.console.log(members) }
  
load "http://127.0.0.1:10051" |> Async.StartImmediate  

let run () =

  let services = createEmpty<editor.IEditorOverrideServices>

  let lang = createEmpty<languages.ILanguageExtensionPoint>
  lang.id <- "thegamma"

  let noState = 
    { new languages.IState with
        member this.clone() = this
        member this.equals(other) = true }

  let toks = 
    { new monaco.languages.TokensProvider with
        member this.tokenize(line, state) =
          let tokens = createEmpty<languages.ILineTokens>
          tokens.endState <- noState
          tokens.tokens <- ResizeArray()

          let (Parsec.Parser p) = Tokenizer.tokens
          match p (List.ofSeq line) with
          | Some(_, r) ->
              //Browser.console.log(line)
              //Browser.console.log(r)
              for t in r do
                let tok = createEmpty<languages.IToken>
                tok.startIndex <- float t.Range.Start.Column
                tok.scopes <- U2.Case1 (match t.Token with TokenKind.QIdent _ | TokenKind.Ident _ -> "entity" | TokenKind.Dot _ -> "operator" | TokenKind.Boolean _ -> "keyword" | TokenKind.Number _ -> "number" | _ -> "")
                tokens.tokens.Add(tok)
          | _ -> ()

          tokens
        member this.getInitialState() = noState }

  monaco.languages.Globals.setTokensProvider("thegamma", toks) |> ignore

  let rec seriesTy() = 
    { new Future<_> with
        member x.Then(f) = 
          Type.Object 
            { Members = 
              [ Member.Method("sortValues", ["reverse", Type.Primitive "bool"], seriesTy ())
                Member.Method("take", ["count", Type.Primitive "num"], seriesTy ()) ] } |> f } |> Delayed

  let worldTy = 
    Type.Object
      { Members = 
          [ Member.Property("CO2 emissions (kt)", seriesTy ()) ] }

  let globals = Map.ofSeq [ "world", worldTy ]

  let typeCheck (text:string) = 
    let sample = text.Replace("\r\n", "\n")
    let (Parsec.Parser p) = Tokenizer.tokens
    match p (List.ofSeq sample) with
    | Some([], r) ->
        let r = r |> List.filter (function { Token = TokenKind.White _ } -> false | _ -> true)
        let (Parsec.Parser q) = Parser.expression
        match q r with
        | Some(_, r) -> // first should be empty - but whatever, ignore '.' at the end
            let checkd = TypeChecker.typeCheck { Globals = globals } r
            checkd
        | n -> failwith (sprintf "Parser faield: %A" n)
    | n ->
      failwith (sprintf "Tokenizer faield: %A" n)

  let needsEscaping (s:string) = 
    s.ToCharArray() |> Array.exists (fun c -> not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))  )

  let compr = 
    { new languages.CompletionItemProvider with 
        member this.triggerCharacters = Some(ResizeArray [ "." ])
        member this.provideCompletionItems(model, position, token) =           
          async {          
            let! ty = typeCheck (model.getValue(editor.EndOfLinePreference.LF, false))
            Browser.console.log(ty)

            let getObjectType e = (FsOption.get e).Type |> TypeChecker.asObjectType
            let tb = TypeChecker.typeBefore { Line=int position.lineNumber; Column=int position.column-1 } None ty 
            let! obj = getObjectType tb

            Browser.console.log(obj)

            let completion =
              [ for m in obj.Members ->
                  let ci = createEmpty<languages.CompletionItem>
                  let n, k =
                    match m with 
                    | Member.Method(n, _, _) -> n, languages.CompletionItemKind.Method
                    | Member.Property(n, _) -> n, languages.CompletionItemKind.Property
                  ci.kind <- k
                  ci.label <- n
                  if needsEscaping n then ci.insertText <- Some("'" + n + "'")
                  ci ] 
            return ResizeArray(completion) } |> Async.StartAsPromise |> U4.Case2

        member this.resolveCompletionItem(item, token) = U2.Case1 item }

  monaco.languages.Globals.registerCompletionItemProvider("thegamma", compr)
  |> ignore

  monaco.languages.Globals.register(lang)


  let sample = """world.'CO2 emissions (kt)'
  .sortValues(reverse=true)
  .take(42)"""

  let options = createEmpty<editor.IEditorConstructionOptions>
  options.value <- Some sample
  options.language <- Some "thegamma"

  let editor = monaco.editor.Globals.create(Browser.document.getElementById("container"), options, services)
  ()
(*
  editor.getModel().onDidChangeContent(fun ch ->
    let text = editor.getModel().getValue()

    let sample = text.Replace("\r\n", "\n")
    let (Parsec.Parser p) = Tokenizer.tokens
    match p (List.ofSeq sample) with
    | Some([], r) ->
        let r = r |> List.filter (function { Token = TokenKind.White _ } -> false | _ -> true)
        let (Parsec.Parser q) = Parser.expression
        Browser.console.log(q r)
    | n ->
      failwith (sprintf "Tokenizer faield: %A" n)
  ) |> ignore
*)

run ()
