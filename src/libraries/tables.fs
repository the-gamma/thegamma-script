namespace TheGamma

open System
open TheGamma
open TheGamma.Series
open TheGamma.Html
open Fable.Import.Browser

type Emit = Fable.Core.EmitAttribute

module TableHelpers =
  [<Emit("document.getElementById(outputElementID)")>]
  let outputElement() : HTMLDivElement = failwith "!"
  [<Emit("blockCallback()")>]
  let invokeBlockCallback() : unit = failwith "!"

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

type table<'k,'v> =
  { data : series<'k,'v> }

  static member create(data:series<_, _>) =
    { table.data = data }

  member t.show() =
    let row (el:string) (things:seq<string>) =
      h?tr [] [ for t in things -> h?(el) [] [text t] ]

    let render nd = 
      nd |> renderTo (outputElement())

    let makeTable header body = 
      h?table ["class" => "table table-striped"] [
        h?caption [] [ text t.data.seriesName ]
        h?thead [] [ row "th" header ]
        h?tbody [] body
      ]

    [ h?tr [] [ h?td ["colspan" => "2"] [text "Loading data..."] ] ]
    |> makeTable [ t.data.keyName; t.data.valueName ]
    |> render

    invokeBlockCallback()

    async {
      let! vs = t.data.data

      let _, first = vs |> Seq.head
      let headers = 
        if isObject first then t.data.keyName::[ for kv in properties first -> kv.key ]
        else [ t.data.keyName; t.data.valueName ]
      
      [ for k, v in vs ->
          let formattedVals =
            if isObject v then [ for kv in properties v -> unbox kv.value ]
            elif not (isNumber v) then [ v.ToString() ]
            elif isNaN (unbox v) then [ "" ]
            else [ formatNumber (unbox v) "0,0.00" ]
          row "td" (unbox k :: formattedVals) ]
      |> makeTable headers
      |> render }
    |> Async.StartImmediate

type empty =
  static member show() =
    outputElement().innerHTML <- """<div class="loading"><p>No output produced.</p></div>"""
    invokeBlockCallback()
