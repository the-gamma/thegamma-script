namespace TheGamma

open System
open TheGamma
open TheGamma.Series
open Fable.Core

(*
type Table<'k,'v> =
  { data : series<'k,'v> }

[<ReflectedDefinition>]
type table =
  static member create(data:series<_, _>) =
    { Table.data = data }

[<ReflectedDefinition>]
module TableHelpers =
  [<Emit("document.getElementById(outputElementID)")>]
  let outputElement() : HTMLDivElement = failwith "!"
  [<Emit("blockCallback()")>]
  let invokeBlockCallback() : unit = failwith "!"

  [<Emit("numeral({0}).format({1})")>]
  let formatNumber (n:float) (format:string) : string = failwith "!"

  [<Emit("(typeof({0})=='number')")>]
  let isNumber(n:obj) : bool = failwith "!"

  [<Emit("isNaN({0})")>]
  let isNaN(n:float) : bool = failwith "!"

  let jq (s:obj) =
    Globals.jQuery.Invoke(unbox<string> s)

open TableHelpers

type Table<'k, 'v> with
  [<ReflectedDefinition>]
  member t.show() =
    let table = jq("<table class='table table-striped' />")
    let row (el:string) (things:string[]) =
      let tr = jq("<tr />")
      for t in things do
        jq(el).text(t).appendTo(tr) |> ignore
      tr

    jq("<caption />").text(t.data.seriesName).appendTo(table) |> ignore

    let th = jq("<thead />").appendTo(table)
    (row "<th />" [| t.data.keyName; t.data.valueName |]).appendTo(th) |> ignore

    let tb = jq("<tbody />").appendTo(table)
    jq("<tr><td colspan='2'>Loading data...</td></tr>").appendTo(tb) |> ignore

    jq(outputElement()).empty().append(table) |> ignore
    invokeBlockCallback()

    async {
      let! vs = t.data.data
      tb.empty() |> ignore
      for k, v in vs do
        let formattedVal =
          if not (isNumber v) then v.ToString() 
          elif isNaN (unbox v) then "" 
          else formatNumber (unbox v) "0,0.00"
        (row "<td />" [| unbox k; formattedVal |]).appendTo(tb) |> ignore }
    |> Async.StartImmediate

[<ReflectedDefinition>]
type empty =
  static member show() =
    outputElement().innerHTML <- """<div class="loading"><p>No output produced.</p></div>"""
    invokeBlockCallback()
    *)