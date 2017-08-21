// --------------------------------------------------------------------------------------------------------------------
// Google chart API
// --------------------------------------------------------------------------------------------------------------------
namespace TheGamma.GoogleCharts

open TheGamma.Common
open TheGamma.Series
open Fable.Core
open Fable.Import

module GoogleCharts = 
  type DataTable =
    abstract addRows : obj[][] -> unit
    abstract addColumn : string * string -> unit 
    
  [<Emit("new google.visualization.DataTable()")>]
  let createTable() : DataTable = failwith "Never"


type ChartData =
  { data : Async<GoogleCharts.DataTable> }

type Chart = interface end

module LazyCharting = 
  let chartsToDraw = ResizeArray<_>()
  let mutable googleLoaded = false

  let drawChartOnLoad f = 
    if googleLoaded then f()
    else chartsToDraw.Add(f)

  [<Emit("""
    if (typeof google != "undefined")
      google.load('visualization', '1', { 'packages': ['corechart'], 'callback': function() { $0(); } });
  """)>]
  let initGoogle (f:unit -> unit) : unit = failwith "JS"

  do initGoogle (fun () ->
    googleLoaded <- true
    for f in chartsToDraw do f() )

  [<Emit("""
    var ctor = eval("(function(a) { return new google.visualization." + $0.typeName + " (a); })");
    var ch = ctor(document.getElementById($2));
    if ($0.options.height == undefined) $0.options.height = 400;
    ch.draw($1, $0.options);""")>]
  let drawChart (chart:#Chart) (data:GoogleCharts.DataTable) (id:string) : unit = failwith "JS"

module Helpers =

  [<Emit("undefined")>]
  let undefined<'T>() : 'T = failwith "!"

  [<Emit("$0==null")>]
  let isNull(o:obj) : bool = failwith "never"

  [<Emit("$0[$1]")>]
  let getProperty<'T> (obj:obj) (name:string) : 'T = failwith "never"

  let copy o prop =
    if isNull o then undefined<_>() else getProperty o prop

  let orDefault newValue =
    match newValue with
    | Some a -> a
    | _ -> undefined<_>()

  let right o prop newValue =
    match newValue with
    | Some a -> a
    | _ when isNull o -> undefined<_>()
    | _ -> getProperty o prop

  let showChart (chart:#Chart) (outputId:string) =
    LazyCharting.drawChartOnLoad(fun () ->
      async {
        try
          let! dt = (getProperty<ChartData> chart "data").data
          LazyCharting.drawChart chart dt outputId 
        with e ->
          Log.error("google", "Error when getting data or rendering chart: %O", e) }
        |> Async.StartImmediate)

module ChartDataOperations =
  let rec collect f l = async {
    match l with 
    | x::xs -> 
        let! y = f x
        let! ys = collect f xs
        return List.append y ys
    | [] -> return [] }

  let oneKeyValue keyType (v:series<'k, float>) = { data = async {
    let data = GoogleCharts.createTable()
    data.addColumn(keyType, v.keyName) |> ignore
    data.addColumn("number", v.seriesName) |> ignore
    let! vals = v.mapPairs(fun k v -> [| box k; box v |]).data |> Async.AwaitFuture
    vals |> Array.map snd |> data.addRows |> ignore
    return data } }

  let oneKeyTwoValues keyType (v:series<'k, float * float>) = { data = async {
    let data = GoogleCharts.createTable()
    data.addColumn(keyType, v.keyName) |> ignore
    data.addColumn("number", v.seriesName) |> ignore
    data.addColumn("number", v.seriesName) |> ignore
    let! vals = v.mapPairs(fun k (v1, v2) -> [| box k; box v1; box v2 |]).data |> Async.AwaitFuture
    vals |> Array.map snd |> data.addRows |> ignore
    return data } }

  let oneKeyAppendValues keyType (vs:series<'k, float>[]) colors = { data = async {
    let data = GoogleCharts.createTable()
    data.addColumn(keyType, vs.[0].keyName) |> ignore
    data.addColumn("number", vs.[0].valueName) |> ignore
    JsInterop.(?) data "addColumn" (JsInterop.createObj [ "type", box "string"; "role", box "style" ]) |> ignore    
    let! all = Array.zip vs colors |> List.ofArray |> collect (fun (v, clr) -> async {
      let! res = v.mapPairs(fun k v -> k, v, clr).data |> Async.AwaitFuture
      return res |> Array.map snd |> List.ofArray })

    all 
    |> List.sortByDescending (fun (_, v, _) -> v) |> Array.ofList
    |> Array.map (fun (k, v, c) -> [| box k; box v; box c |])
    |> data.addRows 

    return data } }

(*
  let oneKeyNValues keyType (v:seq<series<'k, float>>) = { data = async {
    let data = GoogleCharts.createTable()
    let v = Array.ofSeq v
    data.addColumn(keyType, v.[0].keyName) |> ignore
    for i in 0 .. v.Length - 1 do
      data.addColumn("number", v.[i].seriesName) |> ignore

    let head = v.[0].map(fun v -> Map.ofList [0,v])
    let tail = SeriesInternals.slice 1 (v.Length-1) v |> Array.mapi (fun i v -> i+1, v)
    let all = (head,tail) ||> Array.fold (fun s1 (i, s2) ->
      s1.joinOuter(s2).map(fun (l, r) ->
        match defaultArg l Map.empty, r with
        | lm, Some r -> Map.add i r lm
        | lm, None -> lm ))

    let! vals = all.mapPairs(fun k vals ->
      let data = Array.init v.Length (fun i -> box (defaultArg (Map.tryFind i vals) (Helpers.undefined<_>())))
      Array.append [| box k |] data).data
    vals |> Array.map snd |> data.addRows |> ignore
    return data } }
*)
  let oneKeyNValues keyType (v:series<'a, series<'k, float>>) = { data = async {
    let data = GoogleCharts.createTable()
    let! v = v.data |> Async.AwaitFuture
    let v = Array.map snd v
    data.addColumn(keyType, v.[0].keyName) |> ignore
    for i in 0 .. v.Length - 1 do
      data.addColumn("number", v.[i].seriesName) |> ignore

    let head = v.[0].map(fun v -> Map.ofList [0,v])
    let tail = SeriesInternals.slice 1 (v.Length-1) v |> Array.mapi (fun i v -> i+1, v)
    let all = (head,tail) ||> Array.fold (fun s1 (i, s2) ->
      s1.joinOuter(s2).map(fun (l, r) ->
        match defaultArg l Map.empty, r with
        | lm, Some r -> Map.add i r lm
        | lm, None -> lm ))

    let! vals = all.mapPairs(fun k vals ->
      let data = Array.init v.Length (fun i -> box (defaultArg (Map.tryFind i vals) (Helpers.undefined<_>())))
      Array.append [| box k |] data).data |> Async.AwaitFuture
    vals |> Array.map snd |> data.addRows |> ignore
    return data } }

  let twoValues (v1:series<'k, float>) (v2:series<'k,float>) = { data = async {
    let data = GoogleCharts.createTable()
    data.addColumn("number", v1.seriesName) |> ignore
    data.addColumn("number", v2.seriesName) |> ignore
    let! vals = v1.joinInner(v2).map(fun v -> [| box v.first; box v.second |]).data |> Async.AwaitFuture
    vals |> Array.map snd |> data.addRows |> ignore
    return data } }
