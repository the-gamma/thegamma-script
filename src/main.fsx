#r "../paket-files/github.com/fsprojects/Fable/build/fable/bin/Fable.Core.dll"
#r "libraries/bin/Debug/libraries.dll"
#r "thegamma/bin/Debug/thegamma.dll"
#r "bindings/bin/Debug/bindings.dll"
open Fable.Core
open Fable.Core.Extensions
open Fable.Import
open Fable.Import.Browser
open Fable.Import.monaco
open Fable.Extensions
module FsOption = Microsoft.FSharp.Core.Option

open TheGamma
open TheGamma.Babel

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
    [ TypePoviders.RestProvider.provideRestType lookupNamed "olympics" "http://127.0.0.1:10042/pivot/olympics" 
      TypePoviders.RestProvider.provideRestType lookupNamed "adventure" "http://127.0.0.1:10042/adventure"
      TypePoviders.RestProvider.provideRestType lookupNamed "world" "http://127.0.0.1:10042/worldbank" 
      
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
    

let (|ExprLeaf|ExprNode|) e = 
  match e with
  | ExprKind.Property(e, n) -> ExprNode([e], [n])
  | ExprKind.Call(e, n, args) -> ExprNode(e::[for a in args -> a.Value ], n::(args |> List.choose (fun a -> a.Name)))
  | ExprKind.Variable(n) -> ExprNode([], [n])
  | ExprKind.Number _
  | ExprKind.Boolean _
  | ExprKind.Unit
  | ExprKind.Empty -> ExprLeaf()

let rebuildExprNode e es ns =
  match e, es, ns with
  | ExprKind.Property(_, _), [e], [n] -> ExprKind.Property(e, n)
  | ExprKind.Call(_, _, args), e::es, n::ns ->
      let rec rebuildArgs args es ns =
        match args, es, ns with
        | { Argument.Name = None }::args, e::es, ns -> { Value = e; Name = None }::(rebuildArgs args es ns)
        | { Argument.Name = Some _ }::args, e::es, n::ns -> { Value = e; Name = Some n }::(rebuildArgs args es ns)
        | [], [], [] -> []
        | _ -> failwith "rebuildExprNode: Wrong call length"
      ExprKind.Call(e, n, rebuildArgs args es ns)
  | ExprKind.Variable _, [], [n] -> ExprKind.Variable(n)
  | ExprKind.Variable _, _, _ -> failwith "rebuildExprNode: Wrong variable length"
  | ExprKind.Property _, _, _ -> failwith "rebuildExprNode: Wrong property length"
  | ExprKind.Call _, _, _ -> failwith "rebuildExprNode: Wrong call length"
  | ExprKind.Number _, _, _
  | ExprKind.Boolean _, _, _
  | ExprKind.Empty, _, _ 
  | ExprKind.Unit, _, _ -> failwith "rebuildExprNode: Not a node"

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


type DomNode = 
  | Text of string
  | Element of tag:string * attributes : (string * string)[] * children : DomNode[]

let rec render node = 
  match node with
  | Text(s) -> 
      document.createTextNode(s) :> Node
  | Element(tag, attrs, children) ->
      let el = document.createElement(tag)
      for c in children do el.appendChild(render c) |> ignore
      for k, v in attrs do el.setAttribute(k, v)
      el :> Node

let div a c = Element("div", Array.ofList a, Array.ofList c)
let text s = Text(s)
let (=>) k v = k, v

let renderErrors errors = 
  div ["class" => "error"] 
    [ for (e:Error) in errors -> 
        div [] [
          text (sprintf "%d:%d" e.Range.Start e.Range.End); 
          text "error "; text (string e.Number); text ": "; text (e.Message)] ]

let reportErrors errors = 
  let node = renderErrors errors
  let ediv = document.getElementById("errors")
  while box ediv.lastChild <> null do ignore(ediv.removeChild(ediv.lastChild))
  ediv.appendChild(render node) |> ignore

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

          let _, tokenized = tokenize line
          for t in tokenized do
            let tok = createEmpty<languages.IToken>
            tok.startIndex <- float t.Range.Start
            tok.scopes <- U2.Case1 (match t.Token with TokenKind.QIdent _ | TokenKind.Ident _ -> "" | TokenKind.Dot _ -> "operator" | TokenKind.Let | TokenKind.Boolean _ -> "keyword" | TokenKind.Number _ -> "number" | _ -> "")
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
              let! info = TypeChecker.collectProgramInfo { Completions = [] } ty
            
              let absPosition = 
                int position.column - 1 +
                  List.fold (+) 0 [ for i in 1 .. int position.lineNumber-1 -> lines.[i-1].Length + 1 ]
  
              let optMembers = 
                info.Completions 
                |> List.filter (fun (rng, _) -> absPosition >= rng.Start && absPosition <= rng.End)
                |> List.sortBy (fun (rng, _) -> -rng.Start)
                |> List.tryHead

              match optMembers with 
              | None -> return ResizeArray []
              | Some (_, members) -> 
                let completion =
                  [ for m in members ->
                      let ci = createEmpty<languages.CompletionItem>
                      let n, k =
                        match m with 
                        | Member.Method(name=n) -> n, languages.CompletionItemKind.Method
                        | Member.Property(name=n) -> n, languages.CompletionItemKind.Property
                      ci.kind <- k
                      ci.label <- n
                      if needsEscaping n then ci.insertText <- Some("'" + n + "'")
                      ci ] 
                Browser.console.log("Completions: %O", completion)
                return ResizeArray(completion)
              with e ->
                Browser.console.log("*** Type checking failed ***")
                Browser.console.log(e) 
                return ResizeArray [] } |> Async.StartAsPromise |> U4.Case2

        member this.resolveCompletionItem(item, token) = U2.Case1 item }

  monaco.languages.Globals.registerCompletionItemProvider("thegamma", compr)
  |> ignore

  monaco.languages.Globals.register(lang)


  let sample1 = """let data =
  world
    .byCountry.China
    .'Climate Change'.'CO2 emissions (kt)'
    .take(10)

chart.show(chart.column(data))"""
  let sample = """let phelps = 
  olympics.'by athlete'.'United States'  
    .'PHELPS, Michael'.data

table.create(phelps).show()"""

  let options = createEmpty<editor.IEditorConstructionOptions>
  options.value <- Some sample
  options.language <- Some "thegamma"

  let ed = monaco.editor.Globals.create(Browser.document.getElementById("container"), options, services)
  ()

  ed.getModel().onDidChangeContent(fun ch ->
    let text = ed.getModel().getValue(editor.EndOfLinePreference.LF, false)
    
    async {
      try
        let! errs, expr = typeCheck text 
        reportErrors errs

        (*Browser.console.log("*** Type checking completed ***")
        for e in errs do
          Browser.console.log("Error %s: %s (%O)", e.Number, e.Message, e.Range)
        Browser.console.log("Expression: %O", expr) 
        *)
      with e ->
        Browser.console.log("*** Type checking failed ***")
        Browser.console.log(e) } |> Async.StartImmediate

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
        TypePovidersRuntime.trimLeft |> ignore
        TheGamma.GoogleCharts.chart.bar |> ignore
        TheGamma.table.create(s) |> ignore

        evalCode code.code

        // let lengths = 

      with e ->
        Browser.console.log("*** Type checking failed ***")
        Browser.console.log(e) } |> Async.StartImmediate

    box()

run ()


let fooooo () =
  let rc = TheGamma.TypePovidersRuntime.RuntimeContext("lol", "troll")
  rc.addTrace("zzz").getValue("yollo")
