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
open TheGamma.TypeProviders

let transformName = function
  | Pivot.DropColumns _ -> "drop columns"
  | Pivot.Empty _ -> "empty"
  | Pivot.FilterBy _ -> "filter by"
  | Pivot.GetSeries _ -> "get series"
  | Pivot.GetTheData _ -> "get the data"
  | Pivot.GroupBy _ -> "group by"
  | Pivot.Paging _ -> "paging"
  | Pivot.SortBy _ -> "sort by"

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
  | Expr.Variable(n) -> Some((n.Range.Start, node)::acc)
  | _ -> None

let rec collectFirstChain expr = 
  match collectChain [] expr with
  | Some((_::_) as chain) -> Some(id, chain)
  | _ ->
  match expr with
  | { Node = ExprNode(es, ns) } -> 
      let rec loop acc es =   
        match es with 
        | [] -> None
        | e::es ->
            match collectFirstChain e with 
            | None -> loop (e::acc) es
            | Some(recreate, chain) ->
                let recreate newChain =
                  { expr with Node = rebuildExprNode e.Node (List.rev acc @ [ recreate newChain ] @ es) ns }
                Some(recreate, chain)
      loop [] es
  | _ -> None

// ------------------------------------------------------------------------------------------------
// Elmish pivot editor
// ------------------------------------------------------------------------------------------------

type PivotEditorMenus =
  | AddDropdownOpen
  | ContextualDropdownOpen
  | Hidden

type PivotEditorAction = 
  | InitializeGlobals of seq<Entity>
  | UpdateSource of string * int * Program * Monaco.LocationMapper
  | UpdateLocation of int
  | SelectRange of Range
  | SelectChainElement of int
  | AddTransform of Pivot.Transformation
  | RemoveSection of Symbol
  | RemoveElement of Symbol
  | ReplaceElement of Symbol * string * option<list<Expr>>
  | ReplaceRange of Range * string
  | AddElement of Symbol * string * option<list<Expr>>
  | SwitchMenu of PivotEditorMenus

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

let editorLocation (mapper:Monaco.LocationMapper) startIndex endIndex = 
  let sl, sc = mapper.AbsoluteToLineCol(startIndex)
  let el, ec = mapper.AbsoluteToLineCol(endIndex)
  let rng = JsInterop.createEmpty<monaco.IRange>
  rng.startLineNumber <- float sl
  rng.startColumn <- float sc
  rng.endLineNumber <- float el
  rng.endColumn <- float ec
  rng

let selectName nd state = 
  let rng =
    match nd with
    | { Node = Expr.Call(_, n, _) | Expr.Property(_, n) } -> n.Range
    | _ -> nd.Range
  let loc = editorLocation state.Mapper rng.Start (rng.End+1)
  { state with Selection = Some loc }

let tryTransformChain f state = 
  match state.Body with
  | Some body ->
      match collectFirstChain body with
      | Some(recreate, chain) ->
          let sections = chain |> List.map snd |> createPivotSections 
          f body recreate (List.map snd chain) sections |> hideMenus
      | _ -> hideMenus state
  | _ -> hideMenus state

let marker = "InsertPropertyHere"

let replaceAndSelectMarker newName state = 
  let startIndex = state.Code.IndexOf(marker)
  let newCode = state.Code.Replace(marker, Ast.escapeIdent newName)
  let mapper = Monaco.LocationMapper(state.Code)
  let rng = editorLocation mapper startIndex (startIndex + (Ast.escapeIdent newName).Length)
  { state with Code = newCode; Selection = Some rng }

let reconstructChain state (body:Node<_>) newNodes = 
  let newBody =
    List.tail newNodes 
    |> List.fold (fun prev part ->
      match part.Node with 
      | Expr.Property(_, n) -> { part with Node = Expr.Property(prev, n) }
      | Expr.Call(_, n, args) -> { part with Node = Expr.Call(Some prev, n, args) }
      | _ -> failwith "Unexpected node in call chain") (List.head newNodes)

  let newCode = (Ast.formatSingleExpression newBody).Trim()
  let newCode = state.Code.Substring(0, body.Range.Start) + newCode + state.Code.Substring(body.Range.End + 1)
  { state with Code = newCode }

let createChainNode args = 
  let node nd = Ast.node {Start=0; End=0} nd
  match args with
  | None -> node (Expr.Property(node Expr.Empty, node {Name=marker}))
  | Some args -> 
      let args = args |> List.map (fun a -> { Name = None; Value = node a })
      node (Expr.Call(None, node {Name=marker}, node args))

let updatePivotState state event = 
  match event with
  | InitializeGlobals(globals) ->
      { state with PivotEditorState.Globals = globals }
  | UpdateLocation(loc) ->
      { state with Location = loc } |> updateBody |> hideMenus
  | UpdateSource(code, loc, program, mapper) ->
      { state with Location = loc; Program = program; Code = code; Mapper = mapper } |> updateBody |> hideMenus
  | SwitchMenu menu ->
      { state with Menus = menu }

  | SelectChainElement(dir) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let rec loop before (chain:Node<_> list) = 
          match chain with
          | c::chain when c.Range.End + 1 < state.Location -> loop c chain
          | c::after::_ -> before, c, after
          | [c] -> before, c, c
          | [] -> before, before, before
        let before, it, after = loop (List.head chain) (List.tail chain)
        state |> selectName (if dir < 0 then before elif dir > 0 then after else it)
      )
          
  | SelectRange(rng) ->    
      { state with Selection = Some (editorLocation state.Mapper rng.Start (rng.End+1)) }

  | ReplaceRange(rng, value) ->    
      { state with
          Code = state.Code.Substring(0, rng.Start) + value + state.Code.Substring(rng.End + 1)
          Selection = Some (editorLocation state.Mapper rng.Start (rng.Start+value.Length)) }

  | AddElement(sym, name, args) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let newNodes =
          chain |> List.collect (fun nd -> 
            if nd.Entity.Value.Symbol <> sym then [nd]
            else [nd; createChainNode args] )        
        reconstructChain state body newNodes
        |> replaceAndSelectMarker name
      )

  | ReplaceElement(sym, name, args) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let newNodes =
          chain |> List.map (fun nd -> 
            if nd.Entity.Value.Symbol <> sym then nd
            else createChainNode args )
        reconstructChain state body newNodes
        |> replaceAndSelectMarker name
      )

  | RemoveElement sym ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let beforeDropped = chain |> List.takeWhile (fun nd -> nd.Entity.Value.Symbol <> sym) |> List.tryLast
        let beforeDropped = defaultArg beforeDropped (List.head chain)
        let newNodes = chain |> List.filter (fun nd -> nd.Entity.Value.Symbol <> sym)        
        reconstructChain state body newNodes
        |> selectName beforeDropped
      )

  | RemoveSection sym ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let beforeDropped = 
          sections |> List.map (fun sec -> List.head sec.Nodes) 
          |> List.takeWhile (fun nd -> nd.Entity.Value.Symbol <> sym) |> List.tryLast
        let beforeDropped = defaultArg beforeDropped (List.head chain)
        let newNodes = sections |> List.filter (fun sec -> (List.head sec.Nodes).Entity.Value.Symbol <> sym)
        let newNodes = newNodes |> List.collect (fun sec -> sec.Nodes)
        reconstructChain state body newNodes
        |> selectName beforeDropped 
      )

  | AddTransform tfs ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let whites =
          sections 
          |> List.map (fun sec -> Ast.formatWhiteAfterExpr (List.last sec.Nodes))
          |> List.countBy id
        let whiteAfter, _ = ("",0)::whites |> List.maxBy (fun (s, c) -> if s = "" then 0 else c)
        Log.trace("live", "Whitespace of sections: %O, inserting '%s'", Array.ofList whites, whiteAfter)
        let node n = Ast.node { Start=0; End=0; } n

        let firstProperty, properties = 
          match tfs with
          | Pivot.DropColumns _ -> "drop columns", [marker; "then"]
          | Pivot.SortBy _ -> "sort data", [marker; "then"]
          | Pivot.GroupBy _ -> "group data", [marker; "by ???"; "then"] // Wrong - pick group key
          | Pivot.FilterBy _ -> "filter data", [marker; "then"] // Wrong - add then to pivot
          | Pivot.Paging _ -> "paging", [marker; "then"] // Wrong - add then to pivot
          | Pivot.GetSeries _ -> "", [] // Wrong - we need to drop 
          | Pivot.GetTheData _ | Pivot.Empty -> "", []

        let injectCall expr = 
          properties |> List.fold (fun expr name ->
            node (Expr.Property(expr, node { Name = name }))) expr
          |> Parser.whiteAfter [ { Token = TokenKind.White whiteAfter; Range = {Start=0; End=0} } ]

        let tryInjectBefore prev part =
          match pickPivotChainElement part with
          | Some (Pivot.GetSeries _ :: _) 
          | Some (Pivot.GetTheData _ :: _) -> true, injectCall prev
          | _ -> false, prev

        let injected, newBody =
          List.tail chain |> List.fold (fun (injected, prev) part ->
            let injected, prev = tryInjectBefore prev part
            match part.Node with 
            | Expr.Property(_, n) -> injected, { part with Node = Expr.Property(prev, n) }
            | Expr.Call(_, n, args) -> injected, { part with Node = Expr.Call(Some prev, n, args) }
            | _ -> failwith "Unexpected node in call chain") (false, List.head chain)

        let newBody = recreate (if injected then newBody else injectCall newBody)
        let newCode = (Ast.formatSingleExpression newBody).Trim()
        let newCode = state.Code.Substring(0, body.Range.Start) + newCode + state.Code.Substring(body.Range.End + 1)
        { state with Code = newCode } |> replaceAndSelectMarker firstProperty 
      )

let renderPivot triggerEvent state = 
  let trigger action = fun _ (e:Event) -> e.cancelBubble <- true; triggerEvent action
  match state.Body with
  | None -> None 
  | Some body ->
  match collectFirstChain body with
  | None -> None
  | Some(_, chainNodes) ->
      let starts = [| for r,n in chainNodes -> sprintf "%d: %s" r n.Entity.Value.Name |]
      Log.trace("live", "Find chain element at %d in %O", state.Location, starts)
      match chainNodes |> List.filter (fun (start, node) -> state.Location >= start) |> List.tryLast with
      | None -> None
      | Some(_, selNode) ->
          let selEnt = selNode.Entity.Value
          let sections = chainNodes |> List.map snd |> createPivotSections 
          let selSec = sections |> List.tryFind (fun sec -> 
            sec.Nodes |> List.exists (fun secEnt -> selEnt.Symbol = secEnt.Entity.Value.Symbol) )
          let preview = defaultArg (tryFindPreview state.Globals selEnt) ignore
          let dom = 
            h?div [
                yield "class" => "pivot-preview"
                if state.Menus <> Hidden then yield "click" =!> trigger (SwitchMenu Hidden)
              ] [
              h?ul ["class" => "tabs"] [
                for sec in sections ->
                  let selected = sec.Nodes |> List.exists (fun secEnt -> selEnt.Symbol = secEnt.Entity.Value.Symbol)
                  let secSymbol = (sec.Nodes |> List.head).Entity.Value.Symbol
                  let identRange = 
                    match sec.Nodes with
                    | { Node = Expr.Variable n | Expr.Call(_, n, _) | Expr.Property(_, n) }::_ -> n.Range
                    | _ -> failwith "Unexpected node in pivot call chain" 

                  h?li ["class" => if selected then "selected" else ""] [ 
                    h?a ["click" =!> trigger (SelectRange(identRange)) ] [
                      text (transformName sec.Transformation) 
                    ]
                    h?a ["click" =!> trigger (RemoveSection(secSymbol))] [
                      h?i ["class" => "fa fa-times"] [] 
                    ]
                  ]
                yield h?li ["class" => if state.Menus = AddDropdownOpen then "add selected" else "add"] [ 
                  h?a ["click" =!> trigger (SwitchMenu AddDropdownOpen) ] [
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
              h?div ["class" => "toolbar"] [
                yield h?span ["class"=>"navig"] [
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement -1); "class" => "fa fa-chevron-left"] [] ]
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement 0); "class" => "fa fa-circle"] [] ]
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement +1); "class" => "fa fa-chevron-right"] [] ]
                ]
                match selSec with
                | Some { Nodes = nodes; Transformation = Pivot.Paging _ } ->                    
                    let methods = nodes |> List.map (function { Node = Expr.Call(_, n, _) } -> n.Node.Name | _ -> "") |> set
                    for nd in nodes do
                      match nd.Node with
                      | Expr.Call(_, n, { Node = [arg] }) ->
                          let removeOp =
                            if n.Node.Name = "take" then ReplaceElement(nd.Entity.Value.Symbol, "then", None)
                            else RemoveElement(nd.Entity.Value.Symbol)
                          yield h?span [] [
                              h?a ["click" =!> trigger (SelectRange(n.Range)) ] [ text n.Node.Name ]
                              h?input [ 
                                "input" =!> fun el _ -> 
                                  triggerEvent (ReplaceRange(arg.Value.Range, (unbox<HTMLInputElement> el).value))
                                "value" => Ast.formatSingleExpression arg.Value ] []
                              h?a ["click" =!> trigger (removeOp)] [
                                h?i ["class" => "fa fa-times"] [] 
                              ]
                            ]
                      | _ -> ()
                    if not (methods.Contains "take" && methods.Contains "skip") then
                      yield h?a ["class" => "right"; "click" =!> trigger (SwitchMenu ContextualDropdownOpen) ] [
                          h?i ["class" => "fa fa-plus"] [] 
                        ]
                | Some { Nodes = nodes; Transformation = Pivot.DropColumns _ } ->
                    for nd in nodes do
                      match nd.Node with
                      | Expr.Property(_, n) when n.Node.Name <> "then" && n.Node.Name <> "drop columns" -> 
                          yield h?span [] [
                              h?a ["click" =!> trigger (SelectRange(n.Range)) ] [
                                text n.Node.Name 
                              ]
                              h?a ["click" =!> trigger (RemoveElement(nd.Entity.Value.Symbol))] [
                                h?i ["class" => "fa fa-times"] [] 
                              ]
                            ]
                      | _ -> ()
                    yield h?a ["class" => "right"; "click" =!> trigger (SwitchMenu ContextualDropdownOpen) ] [
                        h?i ["class" => "fa fa-plus"] [] 
                      ]
                | _ -> ()
              ]

              h?div ["class" => "add-menu"] [
                match state.Menus, selSec with
                | ContextualDropdownOpen, Some { Nodes = nodes; Transformation = Pivot.Paging _ } ->
                    let methods = nodes |> List.choose (function 
                      | { Node = Expr.Property(_, n) | Expr.Call(_, n, _); Entity = e } -> 
                          Some(n.Node.Name, e.Value.Symbol) | _ -> None) |> dict
                    let lastSym = (List.last nodes).Entity.Value.Symbol
                    let firstSym = (List.head nodes).Entity.Value.Symbol
                    yield h?ul [] [
                      if not (methods.ContainsKey "take") then
                        let op = 
                          if methods.ContainsKey "then" then ReplaceElement(methods.["then"], "take", Some [Expr.Number 10.])
                          else AddElement(lastSym, "take", Some [Expr.Number 10.])
                        yield h?li [] [ h?a [ "click" =!> trigger op ] [ text "take"] ]
                      if not (methods.ContainsKey "skip") then
                        let op = AddElement(firstSym, "skip", Some [Expr.Number 10.])
                        yield h?li [] [ h?a [ "click" =!> trigger op ] [ text "skip"] ]
                    ]
                | ContextualDropdownOpen, Some { Nodes = nodes; Transformation = Pivot.DropColumns _ } ->
                    let lastNode = nodes |> List.rev |> List.find (function { Node = Expr.Property(_, n) } -> n.Node.Name <> "then" | _ -> true) 
                    match lastNode.Entity.Value.Type with
                    | Some(Type.Object obj) ->
                        yield h?ul [] [
                          for m in obj.Members do
                            match m with
                            | Member.Property(name=n) when n.StartsWith("drop") ->
                                yield h?li [] [ 
                                  h?a [ "click" =!> trigger (AddElement(lastNode.Entity.Value.Symbol, n, None)) ] [ text n] 
                                ]
                            | _ -> ()
                        ]
                    | _ -> ()
                | _ -> ()
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
      let oldState = pivotState 
      pivotState <- updatePivotState pivotState evt 
      if (match evt with UpdateSource _ -> false | _ -> true) &&
         (oldState.Code <> pivotState.Code) then
        ed.getModel().setValue(pivotState.Code)
      match pivotState.Selection with
      | Some rng ->
          ed.setSelection(rng)
          ed.focus()
          pivotState <- { pivotState with Selection = None }
      | _ -> ()
      updateZones (renderPivot pivotEvent.Trigger pivotState)
    with e ->
      Log.exn("live", "Error when updating state %O with event %O: %O", pivotState, evt, e) )

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
