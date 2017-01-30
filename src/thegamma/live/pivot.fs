module TheGamma.Live.Pivot

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open TheGamma
open TheGamma.Ast
open TheGamma.Html
open TheGamma.Live.Common
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders

module FsOption = Microsoft.FSharp.Core.Option

[<Emit("$0.setCustomValidity($1)")>]
let setCustomValidity (el:obj) (msg:string) : unit = failwith "JS"

// ------------------------------------------------------------------------------------------------
// Pivot editor: Helpers for working with entities
// ------------------------------------------------------------------------------------------------

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

let commandAtLocation loc (program:Program) =
  program.Body.Node |> List.tryFind (fun cmd ->
    cmd.Range.Start <= loc && cmd.Range.End + 1 >= loc)

// ------------------------------------------------------------------------------------------------
// Pivot editor: Splitting pivot transformations into sections
// ------------------------------------------------------------------------------------------------

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
// Pivot editor: State of the editor
// ------------------------------------------------------------------------------------------------

type PivotEditorMenus =
  | AddDropdownOpen
  | ContextualDropdownOpen
  | Hidden

type PivotEditorAction = 
  | UpdatePreview of DomNode
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
  { Body : Node<Expr>
    FirstNode : Node<Expr>
    Preview : DomNode
    Sections : PivotSection list
    SelectedEntity : Entity
    Menus : PivotEditorMenus
    Focus : option<string * int option> }


// ------------------------------------------------------------------------------------------------
// Pivot editor: Handling events triggered by the editor
// ------------------------------------------------------------------------------------------------

let withPivotState (pivotState:PivotEditorState) state =
  { state with State = pivotState }

let findPreview trigger globals (ent:Entity) = 
  let nm = { Name.Name="preview" }
  match FsOption.map Types.reduceType ent.Type with 
  | Some(Type.Object(TypeChecker.FindMethod nm _))
  | Some(Type.Object(TypeChecker.FindProperty nm _)) ->
      let res = Interpreter.evaluate globals ent  
      let res = res |> FsOption.bind (fun p -> p.Preview.Value)
      match res with
      | Some p ->
          Log.trace("live", "Found preview value: %O", p)
          let mutable node = h?div ["class" => "placeholder"] [text "Loading preview..."]
          let mutable returned = false
          async { let! nd = table<int, int>.create(unbox<Series.series<string, obj>> p).render()
                  if returned then trigger (CustomEvent(UpdatePreview nd))
                  else node <- nd
                  Log.trace("live", "Evaluated to a node") } |> Async.StartImmediate
          returned <- true
          Log.trace("live", "After evaluation started: %O", node)
          node
      | _ -> 
          h?div [ "class" => "placeholder" ] [ text "Preview could not be evaluated" ]
  | _ ->
      h?div [ "class" => "placeholder" ] [ text "This block does not have a preview" ]

let updateBody trigger state = 
  match commandAtLocation state.Location state.Program with
  | Some(cmd) ->
      let line, col = state.Mapper.AbsoluteToLineCol(cmd.Range.End + 1)
      let (Command.Expr expr | Command.Let(_, expr)) = cmd.Node 
      match collectFirstChain expr with
      | Some(recreate, chain) ->
          let sections = chain |> List.map snd |> createPivotSections 
          let _, first = chain |> List.head
          match chain |> List.filter (fun (start, node) -> state.Location >= start) |> List.tryLast with
          | Some(_, selNode) when not (List.isEmpty sections) ->
              let preview = findPreview trigger state.Globals selNode.Entity.Value
              let ps = { Menus = Hidden; Focus = None; FirstNode = first; SelectedEntity = selNode.Entity.Value; Sections = sections; Body = expr; Preview = preview }
              state |> withPivotState ps |> Some
          | _ -> None
      | _ -> None
  | _ -> None

let hideMenus state = 
  { state with State = { state.State with Menus = Hidden } }

let editorLocation (mapper:LocationMapper) startIndex endIndex = 
  let sl, sc = mapper.AbsoluteToLineCol(startIndex)
  let el, ec = mapper.AbsoluteToLineCol(endIndex)
  let rng = JsInterop.createEmpty<monaco.IRange>
  { StartLineNumber = sl; StartColumn = sc 
    EndLineNumber = el; EndColumn = ec }

let selectName nd state = 
  let rng =
    match nd with
    | { Node = Expr.Call(_, n, _) | Expr.Property(_, n) } -> n.Range
    | _ -> nd.Range
  let loc = editorLocation state.Mapper rng.Start (rng.End+1)
  { state with Selection = Some loc }

let tryTransformChain f state = 
  match collectFirstChain state.State.Body with
  | Some(recreate, chain) ->
      let sections = chain |> List.map snd |> createPivotSections 
      f state.State.Body recreate (List.map snd chain) sections |> hideMenus
  | _ -> hideMenus state

let marker = "InsertPropertyHere"

let replaceAndSelectMarker newName state = 
  let startIndex = state.Code.IndexOf(marker)
  let newCode = state.Code.Replace(marker, Ast.escapeIdent newName)
  let mapper = LocationMapper(state.Code)
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

let createChainNode args name = 
  let node nd = Ast.node {Start=0; End=0} nd
  match args with
  | None -> node (Expr.Property(node Expr.Empty, node {Name=name}))
  | Some args -> 
      let args = args |> List.map (fun a -> { Name = None; Value = node a })
      node (Expr.Call(None, node {Name=name}, node args))

let getWhiteBeforeAndAfterSections firstNode sections =
  let dominantWhite whites = 
    let whites = whites |> List.countBy id |> List.ofSeq
    ("",0)::whites |> List.maxBy (fun (s, c) -> if s = "" then 0 else c) |> fst
  let whiteBefore, whiteAfter =
    sections |> List.map (fun sec -> List.head sec.Nodes) |> List.map Ast.formatWhiteBeforeExpr |> dominantWhite,
    sections |> List.map (fun sec -> List.last sec.Nodes) |> List.append [firstNode] |> List.map Ast.formatWhiteAfterExpr |> dominantWhite
  Log.trace("live", "Inserting whitespace before '%s' and after '%s' for sections: %O", whiteBefore, whiteAfter, sections)
  [ { Token = TokenKind.White whiteBefore; Range = {Start=0; End=0} } ],
  [ { Token = TokenKind.White whiteAfter; Range = {Start=0; End=0} } ]

let insertWhiteAroundSection before after section = 
  let lastIdx = (List.length section.Nodes) - 1
  { Transformation = section.Transformation
    Nodes = 
      section.Nodes |> List.mapi (fun i node ->
        let node = 
          match before, node with
          | Some before, { Node = Expr.Property(inst, n) } when i = 0 -> 
              { node with Node = Expr.Property(inst, { n with WhiteBefore = before }) }
          | Some before, { Node = Expr.Call(inst, n, args) } when i = 0 -> 
              { node with Node = Expr.Call(inst, { n with WhiteBefore = before }, args) }
          | _ -> node
        let node = 
          match after, node with
          | Some after, node when i = lastIdx -> { node with WhiteAfter = after }
          | _ -> node
        node ) }

let rec updatePivotState trigger state event = 
  match event with
  | UpdateSource _ 
  | UpdateLocation _ -> state |> hideMenus |> updateBody trigger 
  | InitializeGlobals _ -> Some state
  | CustomEvent event -> 
  match event with
  | UpdatePreview prev -> 
      state |> withPivotState { state.State with Preview = prev } |> Some

  | SwitchMenu menu ->
      state |> withPivotState { state.State with Menus = menu } |> Some
  
  | SelectChainElement(dir) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let rec loop before (chain:Node<_> list) = 
          match chain with
          | c::chain when c.Range.End + 1 < state.Location -> loop c chain
          | c::after::_ -> before, c, after
          | [c] -> before, c, c
          | [] -> before, before, before
        let before, it, after = loop (List.head chain) (List.tail chain)
        state |> selectName (if dir < 0 then before elif dir > 0 then after else it) ) |> Some
          
  | SelectRange(rng) ->    
      { state with Selection = Some (editorLocation state.Mapper rng.Start (rng.End+1)) } |> Some

  | ReplaceRange(rng, value) ->    
      Log.trace("live", "Replace '%s' with '%s'", state.Code.Substring(rng.Start, rng.End - rng.Start + 1), value)
      let newCode = state.Code.Substring(0, rng.Start) + value + state.Code.Substring(rng.End + 1)
      let location = editorLocation (LocationMapper(newCode)) rng.Start (rng.Start+value.Length)
      { state with Code = newCode; Selection = Some location } |> Some

  | AddElement(sym, name, args) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let newNodes =
          chain |> List.collect (fun nd -> 
            if nd.Entity.Value.Symbol <> sym then [nd]
            else [nd; createChainNode args marker] )        
        reconstructChain state body newNodes
        |> replaceAndSelectMarker name) |> Some

  | ReplaceElement(sym, name, args) ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let newNodes =
          chain |> List.map (fun nd -> 
            if nd.Entity.Value.Symbol <> sym then nd
            else createChainNode args marker )
        reconstructChain state body newNodes
        |> replaceAndSelectMarker name) |> Some

  | RemoveElement sym ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let beforeDropped = chain |> List.takeWhile (fun nd -> nd.Entity.Value.Symbol <> sym) |> List.tryLast
        let beforeDropped = defaultArg beforeDropped (List.head chain)
        let newNodes = chain |> List.filter (fun nd -> nd.Entity.Value.Symbol <> sym)        
        reconstructChain state body newNodes
        |> selectName beforeDropped) |> Some

  | RemoveSection sym ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let beforeDropped = 
          sections |> List.map (fun sec -> List.head sec.Nodes) 
          |> List.takeWhile (fun nd -> nd.Entity.Value.Symbol <> sym) |> List.tryLast
        let beforeDropped = defaultArg beforeDropped (List.head chain)
        let newSections = sections |> List.filter (fun sec -> (List.head sec.Nodes).Entity.Value.Symbol <> sym)
        let newNodes = List.head chain :: (newSections |> List.collect (fun sec -> sec.Nodes))
        reconstructChain state body newNodes
        |> selectName beforeDropped ) |> Some

  | AddTransform tfs ->
      state |> tryTransformChain (fun body recreate chain sections ->
        let whiteBefore, whiteAfter = getWhiteBeforeAndAfterSections (List.head chain) sections
        let node n = Ast.node { Start=0; End=0; } n

        let fields =  
          sections 
          |> List.collect (fun s -> s.Nodes) |> List.rev 
          |> List.tryPick pickPivotFields

        let firstProperty, properties = 
          match tfs with
          | Pivot.DropColumns _ -> "drop columns", [marker; "then"]
          | Pivot.SortBy _ -> "sort data", [marker; "then"]
          | Pivot.FilterBy _ -> "filter data", [marker; "then"] // TODO
          | Pivot.Paging _ -> "paging", [marker; "then"]
          | Pivot.GetSeries _ -> "get series", [marker]
          | Pivot.GetTheData -> "get the data", [marker]
          | Pivot.GroupBy(_, _) -> 
              "group data", 
              match fields with
              | Some (f::_) -> [marker; "by " + f.Name; "then"]
              | _ -> [marker; "by Property"; "then"]
          | Pivot.GroupBy([], _) | Pivot.Empty -> "", []

        let newSection =
          { Transformation = tfs; Nodes = List.map (createChainNode None) properties }
          |> insertWhiteAroundSection (Some whiteBefore) (Some whiteAfter)

        let closeFirstSection = function
          | section::sections -> 
              let section =
                match section.Transformation, List.last section.Nodes with
                | Pivot.Paging _, { Node = Expr.Call(_, n, _) } when n.Node.Name = "take" -> section 
                | _, { Node = Expr.Property(_, n) } when n.Node.Name = "then" -> section 
                | _ -> { section with Nodes = section.Nodes @ [createChainNode None "then"] }
              (insertWhiteAroundSection None (Some whiteAfter) section) :: sections
          | [] -> []

        let newSections = 
          match List.rev sections with
          | ({ Transformation = Pivot.GetSeries _ | Pivot.GetTheData } as last)::sections ->
              List.rev (last::newSection::closeFirstSection sections)
          | sections -> List.rev (newSection::closeFirstSection sections)
              
        Log.trace("live", "Inserted section: %O", Array.ofSeq newSections)
        let newNodes = List.head chain :: (newSections |> List.collect (fun sec -> sec.Nodes))
        reconstructChain state body newNodes
        |> replaceAndSelectMarker firstProperty ) |> Some


// ------------------------------------------------------------------------------------------------
// Pivot editor: Rendering user interface from state
// ------------------------------------------------------------------------------------------------

let renderNodeList trigger nodes =
  [ for nd in nodes do
      match nd.Node with
      | Expr.Property(_, n) when n.Node.Name <> "then" ->
          yield h?span [] [
            h?a ["click" =!> trigger (SelectRange(n.Range)) ] [
              text n.Node.Name 
            ]
            h?a ["click" =!> trigger (RemoveElement(nd.Entity.Value.Symbol))] [
              h?i ["class" => "gfa gfa-times"] [] 
            ]
          ]
      | _ -> () ]

let renderContextMenu trigger = 
  h?a ["class" => "right"; "click" =!> trigger (SwitchMenu ContextualDropdownOpen) ] [
    h?i ["class" => "gfa gfa-plus"] [] 
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
  let trigger action = fun _ (e:Event) -> e.cancelBubble <- true; triggerEvent (CustomEvent(action))
  let triggerWith f = fun el (e:Event) -> e.cancelBubble <- true; triggerEvent (CustomEvent(f el))
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
            yield h?span [] [text "with key"]
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
                      if keyName = n then yield "selected" => "selected" ] [ text (n.Replace("with key ", "")) ] 
                | _ -> ()
            ]
        | _ -> ()
        match keyNode with
        | Some({ Entity = Some ({ Type = Some (Type.Object obj) } as keyEnt)}) ->
            yield h?span [] [text "and value"]
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
                      if valName = n then yield "selected" => "selected" ] [ text (n.Replace("and value ", "")) ] 
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
                        ReplaceRange(arg.Value.Range, input.value) |> CustomEvent |> triggerEvent
                      else setCustomValidity el "Cannot parse expression"
                    "value" => Ast.formatSingleExpression arg.Value ] []
                  h?a ["click" =!> trigger (removeOp)] [
                    h?i ["class" => "gfa gfa-times"] [] 
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
                h?i ["class" => "gfa gfa-times"] [] 
              ]
          ]
        yield renderContextMenu trigger

    | Some { Nodes = nodes; Transformation = Pivot.DropColumns _ } ->
        yield! renderNodeList trigger (List.tail nodes)
        yield renderContextMenu trigger

    | _ -> () ]

let renderPivot triggerEvent (state:LiveState<_>) = 
  let trigger action = fun _ (e:Event) -> e.cancelBubble <- true; triggerEvent (CustomEvent(action))
  let triggerWith f = fun el (e:Event) -> e.cancelBubble <- true; triggerEvent (CustomEvent(f el))
  let selSec = state.State.Sections |> List.tryFind (fun sec -> 
    sec.Nodes |> List.exists (fun secEnt -> state.State.SelectedEntity.Symbol = secEnt.Entity.Value.Symbol) )
  let firstNode = state.State.FirstNode
  let dom = 
    h?div [
        yield "class" => "pivot-preview"
        if state.State.Menus <> Hidden then yield "click" =!> trigger (SwitchMenu Hidden)
      ] [
      h?ul ["class" => "tabs"] [
        yield h?li ["class" => if state.State.SelectedEntity.Symbol = firstNode.Entity.Value.Symbol then "selected" else ""] [ 
          h?a ["click" =!> trigger (SelectRange(firstNode.Range)) ] [
            match firstNode.Node with
            | Expr.Variable n -> yield text n.Node.Name
            | _ -> yield text "data"
          ]
        ]
        for sec in state.State.Sections ->
          let selected = sec.Nodes |> List.exists (fun secEnt -> state.State.SelectedEntity.Symbol = secEnt.Entity.Value.Symbol)
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
              h?i ["class" => "gfa gfa-times"] [] 
            ]
          ]
        yield h?li ["class" => if state.State.Menus = AddDropdownOpen then "add selected" else "add"] [ 
          h?a ["click" =!> trigger (SwitchMenu AddDropdownOpen) ] [
            h?i ["class" => "gfa gfa-plus"] [] 
          ]
        ]
      ]
      h?div ["class" => "add-menu"] [
        let clickHandler tfs = "click" =!> trigger (AddTransform(tfs))
        if state.State.Menus = AddDropdownOpen then 
          yield h?ul [] [
            yield h?li [] [ h?a [ clickHandler(Pivot.DropColumns []) ] [ text "drop columns"] ]
            yield h?li [] [ h?a [ clickHandler(Pivot.FilterBy []) ] [ text "filter by"] ]
            yield h?li [] [ h?a [ clickHandler(Pivot.GroupBy([], [])) ] [ text "group by"] ]
            yield h?li [] [ h?a [ clickHandler(Pivot.Paging []) ] [ text "paging"] ]
            yield h?li [] [ h?a [ clickHandler(Pivot.SortBy []) ] [ text "sort by"] ]
            let getDataCalled = 
              state.State.Sections |> List.exists (function 
                | { Transformation = Pivot.GetTheData | Pivot.GetSeries _ } -> true | _ -> false)
            if not getDataCalled then
              yield h?li [] [ h?a [ clickHandler(Pivot.GetTheData) ] [ text "get the data"] ]
              yield h?li [] [ h?a [ clickHandler(Pivot.GetSeries("!", "!")) ] [ text "get series"] ]
          ]
      ]
      h?div ["class" => "toolbar"] [
        yield h?span ["class"=>"navig"] [
          h?a [] [ h?i ["click" =!> trigger (SelectChainElement -1); "class" => "gfa gfa-chevron-left"] [] ]
          h?a [] [ h?i ["click" =!> trigger (SelectChainElement 0); "class" => "gfa gfa-circle"] [] ]
          h?a [] [ h?i ["click" =!> trigger (SelectChainElement +1); "class" => "gfa gfa-chevron-right"] [] ]
        ]
        yield! renderSection triggerEvent selSec
      ]

      h?div ["class" => "add-menu"] [
        match state.State.Menus, selSec with
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
        yield state.State.Preview
      ] 
    ]
  let endLine, _ = state.Mapper.AbsoluteToLineCol(state.State.Body.Range.End)
  Some { Line = endLine; Preview = dom }


// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

let preview = 
  { Update = updatePivotState
    Render = renderPivot 
    InitialState = 
      { Body = Ast.node { Start = 0; End = 0 } Expr.Empty 
        FirstNode = Ast.node { Start = 0; End = 0 } Expr.Empty 
        SelectedEntity = Unchecked.defaultof<_>
        Preview = text "not created"
        Sections = []; Menus = Hidden; Focus = None } }
