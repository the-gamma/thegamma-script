namespace TheGamma

open System
open TheGamma
open TheGamma.Series
open TheGamma.Html
open Fable.Import.Browser

type Emit = Fable.Core.EmitAttribute

module TableHelpers =
  //[<Emit("blockCallback()")>]
  //let invokeBlockCallback() : unit = failwith "!"

  [<Emit("numeral($0).format($1)")>]
  let formatNumber (n:float) (format:string) : string = failwith "!"

  [<Emit("(typeof($0)=='number')")>]
  let isNumber(n:obj) : bool = failwith "!"

  [<Emit("(typeof($0)=='object')")>]
  let isObject(n:obj) : bool = failwith "!"

  [<Emit("isNaN($0)")>]
  let isNaN(n:float) : bool = failwith "!"

  type KeyValue = 
    abstract key : string
    abstract value : obj

  [<Emit("(function(o) { return Object.keys(o).map(function(k) { return {\"key\":k, \"value\":o[k] }; }); })($0)")>]
  let properties(o:obj) : KeyValue[] = failwith "!"

open TableHelpers

type html =
  static member img(url:string) = 
    box (h?img [ "src" => url ] [])

type table<'k,'v> =
  { data : series<'k,'v>
    showKey : bool 
    hiddenColumns : Set<string>
    addedColumns : list<string * ('v -> obj)> }

  static member create(data:series<_, _>) =
    { table.data = data
      hiddenColumns = Set.empty
      addedColumns = []
      showKey = true }

  member t.set(?title:string, ?showKey:bool) = 
    { table.data = t.data.set(t.data.data, seriesName=defaultArg title t.data.seriesName)
      hiddenColumns = t.hiddenColumns
      addedColumns = t.addedColumns
      showKey = defaultArg showKey t.showKey }

  member t.hideColumns(names:string[]) =
    { t with hiddenColumns = Set.ofArray names }

  member t.addColumn(name, f) =
    { t with addedColumns = (name, f)::t.addedColumns }

  member t.show(outputId) =
    let row (el:string) k (things:seq<DomNode>) =
      h?tr [] [ 
        if t.showKey then yield h?(el) [] [text k]
        for t in things -> h?(el) [] [t] 
      ]

    let render nd = 
      nd |> renderTo (document.getElementById(outputId))

    let makeTable k header body = 
      h?table ["class" => "table table-striped"] [
        if not (String.IsNullOrWhiteSpace t.data.seriesName) then
          yield h?caption [] [ text t.data.seriesName ]
        yield h?thead [] [ row "th" k header ]
        yield h?tbody [] body
      ]

    // [ h?tr [] [ h?td ["colspan" => "2"] [text "Loading data..."] ] ]
    // |> makeTable t.data.keyName [ t.data.valueName ]
    // |> render

    //invokeBlockCallback()

    let formatAdded o = 
      // Did someone say hack..?
      let isSeries = 
        [ for kv in properties o -> kv.key ] = 
          ["data"; "keyName"; "valueName"; "seriesName"]
      if isSeries then
        let mutable result = unbox null
        Async.StartWithContinuations
          ( (unbox<series<int, DomNode>> o).data,
            (fun r -> result <- r), ignore, ignore )
        h?span [] (List.ofArray (Array.map snd result))
      else text (o.ToString())

    async {
      try
        let! vs = t.data.data

        let filteredProperties o =
          properties o |> Array.filter (fun kv -> not (t.hiddenColumns.Contains kv.key))

        let _, first = vs |> Seq.head
        let headers = 
          [ if isObject first then for kv in filteredProperties first -> text kv.key
            else yield text t.data.valueName 
            for k, _ in t.addedColumns -> text k ]
      
        [ for k, v in vs ->
            let formattedVals =
              [ if isObject v then for kv in filteredProperties v -> text (unbox kv.value)
                elif not (isNumber v) then yield text (v.ToString())
                elif isNaN (unbox v) then yield text ""
                else yield unbox v  // formatNumber (unbox v) "0,0.00" ]
                for _, f in t.addedColumns -> formatAdded (f v) ] 
            row "td" (unbox k) formattedVals ]
        |> makeTable t.data.keyName headers
        |> render 
      with e ->
        console.log("Getting data for table failed: %O", e) }
    |> Async.StartImmediate

type empty() =
  static member create() = empty()
  member x.show(outputId) =
    h?div ["class" => "loading"] [ h?p [] [ text "No output produced." ] ]
    |> renderTo (document.getElementById(outputId))
