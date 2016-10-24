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

[<Emit("$0.setCustomValidity($1)")>]
let setCustomValidity (el:obj) (msg:string) : unit = failwith "JS"

// ------------------------------------------------------------------------------------------------
// Global provided types
// ------------------------------------------------------------------------------------------------

let services = 
  if isLocalHost() then "http://127.0.0.1:10042/"
  else "http://thegamma-services.azurewebsites.net/"

type ProvidedTypes = 
  { LookupNamed : string -> Type list -> Type
    Globals : list<string * Metadata list * Babel.Expression * Type> }
    
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
        "olympics3" (services + "pivot") ("source=" + services + "olympics")
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "smlouvy1" (services + "smlouvy") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "smlouvy2" (services + "pivot") ("source=" + services + "smlouvy")
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "adventure" (services + "adventure") ""
      TypePoviders.RestProvider.provideRestType lookupNamed 
        "world" (services + "worldbank") ""
      
      TypeProviders.Pivot.providePivotType (services + "pdata/olympics") "olympics" lookupNamed
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
      TypePoviders.NamedType("object", [], Type.Any)
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
    |> List.choose (function TypePoviders.GlobalValue(s, m, e, t) -> Some(s, m, e, t) | _ -> None)
  
  return { Globals = globals; LookupNamed = lookupNamed } } |> Async.StartAsNamedFuture "types"

let globalTypes = async { 
  let! ty = types |> Async.AwaitFuture
  Log.trace("typechecker", "Global values: %O", Array.ofList ty.Globals)
  return ty.Globals |> List.map (fun (n, m, e, t) -> Interpreter.globalEntity n m t (Some e)) } |> Async.StartAsNamedFuture "global types"

let globalExprs = async { 
  let! ty = types |> Async.AwaitFuture
  return ty.Globals |> List.map (fun (n, _, e, _) -> n, e) |> Map.ofList } |> Async.StartAsNamedFuture "global exps"

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

let pickPivotFields expr =
  match expr.Entity with
  | Some { Kind = EntityKind.ChainElement _; Meta = m } 
  | Some { Kind = EntityKind.GlobalValue _; Meta = m } 
  | Some { Kind = EntityKind.Variable(_, { Meta = m }) } -> 
      match pickMetaByType "http://schema.thegamma.net/pivot" "Fields" m with
      | Some m -> Some(unbox<TypeProviders.Pivot.Field list> m)
      | _ -> None
  | _ -> None

let pickPivotTransformations expr =
  match expr.Entity with
  | Some { Kind = EntityKind.ChainElement _; Meta = m } -> 
      match pickMetaByType "http://schema.thegamma.net/pivot" "Transformations" m with
      | Some m -> Some(unbox<TypeProviders.Pivot.Transformation list> m)
      | _ -> None
  | Some { Kind = EntityKind.GlobalValue _; Meta = m } -> 
      Some([])
  | _ -> None

let tryFindPreview globals (ent:Entity) = 
  let nm = {Name.Name="preview"}
  match FsOption.map Types.reduceType ent.Type with 
  | Some(Type.Object(TypeChecker.FindMethod nm _))
  | Some(Type.Object(TypeChecker.FindProperty nm _)) ->
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
    match pickPivotTransformations node with
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
  | SetFocus of string * int option
  | Multiplex of PivotEditorAction list

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
    Menus : PivotEditorMenus
    // Instructing the event loop to do things to the editor  
    Selection : option<monaco.IRange>
    Focus : option<string * int option> }

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

let rec updatePivotState state event = 
  match event with
  | InitializeGlobals(globals) ->
      { state with PivotEditorState.Globals = globals }
  | UpdateLocation(loc) ->
      { state with Location = loc } |> updateBody |> hideMenus
  | UpdateSource(code, loc, program, mapper) ->
      { state with Location = loc; Program = program; Code = code; Mapper = mapper } |> updateBody |> hideMenus
  | SwitchMenu menu ->
      { state with Menus = menu }
  | SetFocus(focus, sel) ->
      { state with Focus = Some(focus, sel) }
  | Multiplex events ->
      events |> List.fold updatePivotState state

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
      Log.trace("live", "Replace '%s' with '%s'", state.Code.Substring(rng.Start, rng.End - rng.Start + 1), value)
      let newCode = state.Code.Substring(0, rng.Start) + value + state.Code.Substring(rng.End + 1)
      let location = editorLocation (Monaco.LocationMapper(newCode)) rng.Start (rng.Start+value.Length)
      { state with Code = newCode; Selection = Some location }

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
        let newNodes = List.head chain :: (newNodes |> List.collect (fun sec -> sec.Nodes))
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

        let firstProperty, getProperties = 
          let res k l = k, fun _ -> l
          match tfs with
          | Pivot.DropColumns _ -> res "drop columns" [marker; "then"]
          | Pivot.SortBy _ -> res "sort data" [marker; "then"]
          | Pivot.FilterBy _ -> res "filter data" [marker; "then"] // TODO
          | Pivot.Paging _ -> res "paging" [marker; "then"]
          | Pivot.GetSeries _ -> res "get series" [marker]
          | Pivot.GetTheData -> res "get the data" [marker]
          | Pivot.GroupBy(_, _) -> "group data", fun expr ->
              Log.trace("live", "Pick fields of %O, got: %O", expr, pickPivotFields expr)
              match pickPivotFields expr with
              | Some (f::_) -> [marker; "by " + f.Name; "then"]
              | _ -> [marker; "by <Property>"; "then"]
          | Pivot.GroupBy([], _) | Pivot.Empty -> res "" []

        let injectCall expr = 
          getProperties expr |> List.fold (fun expr name ->
            node (Expr.Property(expr, node { Name = name }))) expr
          |> Parser.whiteAfter [ { Token = TokenKind.White whiteAfter; Range = {Start=0; End=0} } ]

        let tryInjectBefore prev part =
          match pickPivotTransformations part with
          | Some (Pivot.GetSeries _ :: _) 
          | Some (Pivot.GetTheData _ :: _) -> true, injectCall prev
          | _ -> false, prev

        let injected, newBody =
          List.tail chain |> List.fold (fun (injected, prev) part ->
            let injected, prev = if injected then injected, prev else tryInjectBefore prev part
            match part.Node with 
            | Expr.Property(_, n) -> injected, { part with Node = Expr.Property(prev, n) }
            | Expr.Call(_, n, args) -> injected, { part with Node = Expr.Call(Some prev, n, args) }
            | _ -> failwith "Unexpected node in call chain") (false, List.head chain)

        let newBody = recreate (if injected then newBody else injectCall newBody)
        let newCode = (Ast.formatSingleExpression newBody).Trim()
        let newCode = state.Code.Substring(0, body.Range.Start) + newCode + state.Code.Substring(body.Range.End + 1)
        { state with Code = newCode } |> replaceAndSelectMarker firstProperty 
      )

let renderNodeList trigger nodes =
  [ for nd in nodes do
      match nd.Node with
      | Expr.Property(_, n) when n.Node.Name <> "then" ->
          yield h?span [] [
            h?a ["click" =!> trigger (SelectRange(n.Range)) ] [
              text n.Node.Name 
            ]
            h?a ["click" =!> trigger (RemoveElement(nd.Entity.Value.Symbol))] [
              h?i ["class" => "fa fa-times"] [] 
            ]
          ]
      | _ -> () ]

let renderContextMenu trigger = 
  h?a ["class" => "right"; "click" =!> trigger (SwitchMenu ContextualDropdownOpen) ] [
    h?i ["class" => "fa fa-plus"] [] 
  ]

let renderAddPropertyMenu trigger f nodes =
  [ let lastNode = nodes |> List.rev |> List.find (function { Node = Expr.Property(_, n) } -> n.Node.Name <> "then" | _ -> true) 
    match lastNode.Entity.Value.Type with
    | Some(Type.Object obj) ->
        let members = 
          obj.Members 
          |> Seq.choose (function Member.Property(name=n) when f n -> Some n | _ -> None)
          |> Seq.sort
        yield h?ul [] [
          for n in members ->
            h?li [] [ 
              h?a [ "click" =!> trigger (AddElement(lastNode.Entity.Value.Symbol, n, None)) ] [ text n] 
            ]
        ]
    | _ -> () ]

let renderSection triggerEvent section = 
  let trigger action = fun _ (e:Event) -> e.cancelBubble <- true; triggerEvent action
  let triggerWith f = fun el (e:Event) -> e.cancelBubble <- true; triggerEvent (f el)
  let getNodeNameAndSymbol = function
    | Some { Entity = Some e; Node = Expr.Property(_, n) } -> n.Node.Name, Some e.Symbol
    | _ -> "", None
  [ match section with
    | Some { Nodes = nodes; Transformation = Pivot.GetSeries _ } ->
        let getSeriesNode, keyNode, valNode = 
          match nodes with 
          | gs::gsk::gsv::_ -> gs, Some gsk, Some gsv
          | gs::gsk::_ -> gs, Some gsk, None
          | gs::_ -> gs, None, None
          | _ -> failwith "No get series node in get series transformation"
        let keyName, keySym = getNodeNameAndSymbol keyNode
        let valName, valSym = getNodeNameAndSymbol valNode
        match getSeriesNode.Entity.Value.Type with
        | Some(Type.Object obj) ->
            yield h?select ["change" =!> triggerWith(fun el -> 
                match keySym with
                | None -> AddElement(getSeriesNode.Entity.Value.Symbol, (unbox<HTMLSelectElement> el).value, None)
                | Some selSym -> ReplaceElement(selSym, (unbox<HTMLSelectElement> el).value, None)) ] [
              if keyName = "" then yield h?option [ "value" => ""; "selected" => "selected" ] [ text "" ]
              for m in obj.Members do
                match m with
                | Member.Property(name=n) when n.StartsWith("with key") ->
                    yield h?option [  
                      yield "value" => n
                      if keyName = n then yield "selected" => "selected" ] [ text n ] 
                | _ -> ()
            ]
        | _ -> ()
        match keyNode with
        | Some({ Entity = Some ({ Type = Some (Type.Object obj) } as keyEnt)}) ->
            yield h?select ["change" =!> triggerWith(fun el -> 
                match valSym with
                | None -> AddElement(keyEnt.Symbol, (unbox<HTMLSelectElement> el).value, None)
                | Some selSym -> ReplaceElement(selSym, (unbox<HTMLSelectElement> el).value, None)) ] [
              if valName = "" then yield h?option [ "value" => ""; "selected" => "selected" ] [ text "" ]
              for m in obj.Members do
                match m with
                | Member.Property(name=n) when n.StartsWith("and value") ->
                    yield h?option [  
                      yield "value" => n
                      if keyName = n then yield "selected" => "selected" ] [ text n ] 
                | _ -> ()
            ]
        | _ -> ()

    | Some { Nodes = nodes; Transformation = Pivot.GroupBy _ } ->
        let firstNode, selNode, aggNodes = 
          match nodes with 
          | gby::sel::aggs -> gby, Some sel, aggs
          | gby::_ -> gby, None, []
          | _ -> failwith "No group by node in group by transformation"
        let selName, selSym = getNodeNameAndSymbol selNode
        match firstNode.Entity.Value.Type with
        | Some(Type.Object obj) ->
            yield h?select ["change" =!> triggerWith(fun el -> 
                match selSym with
                | None -> AddElement(firstNode.Entity.Value.Symbol, (unbox<HTMLSelectElement> el).value, None)
                | Some selSym -> ReplaceElement(selSym, (unbox<HTMLSelectElement> el).value, None)) ] [
              if selName = "" then yield h?option [ "value" => ""; "selected" => "selected" ] [ text "" ]
              for m in obj.Members do
                match m with
                | Member.Property(name=n) when n.StartsWith("by") ->
                    yield h?option [  
                      yield "value" => n
                      if selName = n then yield "selected" => "selected" ] [ text n ] 
                | _ -> ()
            ]
        | _ -> ()
        yield! renderNodeList trigger aggNodes  
        yield renderContextMenu trigger

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
                    "id" => "input-pg-" + n.Node.Name
                    "input" =!> fun el _ -> 
                      let input = unbox<HTMLInputElement> el
                      let parsed, errors = Parser.parseProgram input.value
                      if errors.Length = 0 && parsed.Body.Node.Length = 1 then
                        setCustomValidity el ""
                        Multiplex
                          [ SetFocus("input-pg-" + n.Node.Name, Some(int input.selectionStart));
                            ReplaceRange(arg.Value.Range, input.value) ] |> triggerEvent
                      else setCustomValidity el "Cannot parse expression"
                    "value" => Ast.formatSingleExpression arg.Value ] []
                  h?a ["click" =!> trigger (removeOp)] [
                    h?i ["class" => "fa fa-times"] [] 
                  ]
                ]
          | _ -> ()
        if not (methods.Contains "take" && methods.Contains "skip") then
          yield renderContextMenu trigger

    | Some { Nodes = nodes; Transformation = Pivot.SortBy _ } ->
        let props = nodes |> List.choose (function
          | { Node = Expr.Property(_, n); Entity = Some { Symbol = sym} }
              when n.Node.Name <> "then" && n.Node.Name <> "sort data" -> Some(sym, n) | _ -> None)
        let last = List.tryLast props
        for sym, n in props ->
          h?span [] [
            yield h?a ["click" =!> trigger (SelectRange(n.Range)) ] [
              text n.Node.Name 
            ]
            if n.Node.Name = (snd last.Value).Node.Name then
              yield h?a ["click" =!> trigger (RemoveElement(sym))] [
                h?i ["class" => "fa fa-times"] [] 
              ]
          ]
        yield renderContextMenu trigger

    | Some { Nodes = nodes; Transformation = Pivot.DropColumns _ } ->
        yield! renderNodeList trigger (List.tail nodes)
        yield renderContextMenu trigger

    | _ -> () ]

let renderPivot triggerEvent state = 
  let trigger action = fun _ (e:Event) -> e.cancelBubble <- true; triggerEvent action
  let triggerWith f = fun el (e:Event) -> e.cancelBubble <- true; triggerEvent (f el)
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
                let _, firstNode = chainNodes |> List.head
                yield h?li ["class" => if selNode.Entity.Value.Symbol = firstNode.Entity.Value.Symbol then "selected" else ""] [ 
                  h?a ["click" =!> trigger (SelectRange(firstNode.Range)) ] [
                    match firstNode.Node with
                    | Expr.Variable n -> yield text n.Node.Name
                    | _ -> yield text "data"
                  ]
                ]
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
                    yield h?li [] [ h?a [ clickHandler(Pivot.DropColumns []) ] [ text "drop columns"] ]
                    yield h?li [] [ h?a [ clickHandler(Pivot.FilterBy []) ] [ text "filter by"] ]
                    yield h?li [] [ h?a [ clickHandler(Pivot.GroupBy([], [])) ] [ text "group by"] ]
                    yield h?li [] [ h?a [ clickHandler(Pivot.Paging []) ] [ text "paging"] ]
                    yield h?li [] [ h?a [ clickHandler(Pivot.SortBy []) ] [ text "sort by"] ]
                    let getDataCalled = 
                      sections |> List.exists (function 
                        | { Transformation = Pivot.GetTheData | Pivot.GetSeries _ } -> true | _ -> false)
                    if not getDataCalled then
                      yield h?li [] [ h?a [ clickHandler(Pivot.GetTheData) ] [ text "get the data"] ]
                      yield h?li [] [ h?a [ clickHandler(Pivot.GetSeries("!", "!")) ] [ text "get series"] ]
                  ]
              ]
              h?div ["class" => "toolbar"] [
                yield h?span ["class"=>"navig"] [
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement -1); "class" => "fa fa-chevron-left"] [] ]
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement 0); "class" => "fa fa-circle"] [] ]
                  h?a [] [ h?i ["click" =!> trigger (SelectChainElement +1); "class" => "fa fa-chevron-right"] [] ]
                ]
                yield! renderSection triggerEvent selSec
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

                | ContextualDropdownOpen, Some { Nodes = nodes; Transformation = Pivot.GroupBy _ } ->
                    yield! nodes |> renderAddPropertyMenu trigger (fun n -> 
                      n <> "then" && n <> "preview" && not (n.StartsWith("and")) )
                | ContextualDropdownOpen, Some { Nodes = nodes; Transformation = Pivot.SortBy _ } ->
                    yield! nodes |> renderAddPropertyMenu trigger (fun n -> n <> "then" && n <> "preview")
                | ContextualDropdownOpen, Some { Nodes = nodes; Transformation = Pivot.DropColumns _ } ->
                    yield! nodes |> renderAddPropertyMenu trigger (fun n -> n <> "then" && n <> "preview")
                | _ -> ()
              ]
              h?div ["class" => "preview-body"] [
                yield h.delayed preview
              ] 
            ]
          let endLine, _ = state.Mapper.AbsoluteToLineCol(body.Range.End)
          Some(endLine, dom)

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

  let mutable lastCode = ""
  let mutable lastMapper = Monaco.LocationMapper("")
  let mutable changingEditor = false

  let getUpdateEventAfterChange () = async {
    let code = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
    let position = ed.getPosition()
    if code <> lastCode then
      lastCode <- code
      lastMapper <- Monaco.LocationMapper(code)
      let loc = lastMapper.LineColToAbsolute(int position.lineNumber, int position.column)
      let! _, _, program = checker.TypeCheck(code)
      return (UpdateSource(code, loc, program, lastMapper)) 
    else 
      let loc = lastMapper.LineColToAbsolute(int position.lineNumber, int position.column)
      return (UpdateLocation(loc)) }

  let createPivotPreview (ed:monaco.editor.ICodeEditor) = 
    let pivotEvent = new Event<PivotEditorAction>()

    let mutable pivotState = 
      { Selection = None
        Focus = None
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
          changingEditor <- true
          ed.getModel().setValue(pivotState.Code)
        match pivotState.Selection with
        | Some rng ->
            changingEditor <- true
            ed.setSelection(rng)
            pivotState <- { pivotState with Selection = None }
        | _ -> ()

        if changingEditor = true then
          changingEditor <- false
          async { 
            Log.trace("live", "Editor changed. Getting after change event...")
            let! evt = getUpdateEventAfterChange ()
            Log.trace("live", "Editor changed. Updating state %O with event %O", pivotState, evt)
            pivotState <- updatePivotState pivotState evt
            updateZones (renderPivot pivotEvent.Trigger pivotState)
            match pivotState.Focus with 
            | Some(focus, sel) ->
                Log.trace("live", "Set focus to element #%s", focus)
                pivotState <- { pivotState with Focus = None }
                let element = document.getElementById(focus) |> unbox<HTMLInputElement>
                element.focus()
                sel |> FsOption.iter (fun s -> element.selectionStart <- float s; element.selectionEnd <- float s)
            | _ -> () } |> Async.StartImmediate
        else
          updateZones (renderPivot pivotEvent.Trigger pivotState)
      with e ->
        Log.exn("live", "Error when updating state %O with event %O: %O", pivotState, evt, e) )

    async { let! glob = globalTypes |> Async.AwaitFuture 
            pivotEvent.Trigger(InitializeGlobals glob) } |> Async.StartImmediate

    pivotEvent.Trigger

  let trigger = createPivotPreview ed    
      
  do
    ed.onDidChangeCursorPosition(fun ce -> 
      if not changingEditor then
        let code = ed.getModel().getValue(monaco.editor.EndOfLinePreference.LF, false)
        Log.trace("live", "Cursor position changed: code <> lastCode = %s", code <> lastCode)
        async { let! evt = getUpdateEventAfterChange ()
                trigger evt } |> Async.StartImmediate ) |> ignore

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
