namespace TheGamma.Maps

open System
open TheGamma
open TheGamma.Series
open TheGamma.Html
open TheGamma.Common
open Fable.Import.Browser
open Fable.Core

type GeographyConfig = 
  { popupOnHover : bool
    highlightOnHover : bool }
  
type DatamapConfig = 
  { element : HTMLElement
    scope : string
    geographyConfig : GeographyConfig
    fills: obj
    data: obj }

type BubblesConfig = 
  { popupTemplate : System.Func<obj, obj, string>
    key : System.Func<obj, string> }

type IDatamap = 
  abstract bubbles : data:obj * config:BubblesConfig -> unit

module JsDatamap = 
  [<Emit("new Datamap($0)")>]
  let create(config:DatamapConfig) : IDatamap = failwith "JS"

module JsHelpers = 
  [<Emit("$0[$1]")>]
  let getProp (o:obj) (prop:string) : 'T = failwith "JS"

open JsHelpers

(*
var bombMap = new Datamap({
    element: document.getElementById('map'),
    scope: 'world',
    geographyConfig: {
        popupOnHover: false,
        highlightOnHover: false
    },
    fills: {
        'USA': '#1f77b4',
        'RUS': '#9467bd',
        'PRK': '#ff7f0e',
        'PRC': '#2ca02c',
        'IND': '#e377c2',
        'GBR': '#8c564b',
        'FRA': '#d62728',
        'PAK': '#7f7f7f',
        defaultFill: '#EDDC4E'
    },
    data: {
        'RUS': {fillKey: 'RUS'},
        'PRK': {fillKey: 'PRK'},
        'PRC': {fillKey: 'PRC'},
        'IND': {fillKey: 'IND'},
        'GBR': {fillKey: 'GBR'},
        'FRA': {fillKey: 'FRA'},
        'PAK': {fillKey: 'PAK'},
        'USA': {fillKey: 'USA'}
    }
});
 function bombs() { return [{
    name: 'Joe 4',
    radius: Math.random() * 100,
    yield: 400,
    country: 'USSR',
    fillKey: 'RUS',
    significance: 'First fusion weapon test by the USSR (not "staged")',
    date: '1953-08-12',
    latitude: 50.07,
    longitude: 78.43
  },{
    name: 'RDS-37',
    radius: Math.random() * 100,
    yield: 1600,
    country: 'USSR',
    fillKey: 'RUS',
    significance: 'First "staged" thermonuclear weapon test by the USSR (deployable)',
    date: '1955-11-22',
    latitude: 50.07,
    longitude: 78.43
  },{
    name: 'Tsar Bomba',
    radius: Math.random() * 100,
    yield: 50000,
    country: 'USSR',
    fillKey: 'RUS',
    significance: 'Largest thermonuclear weapon ever tested—scaled down from its initial 100 Mt design by 50%',
    date: '1961-10-31',
    latitude: 73.482,
    longitude: 54.5854
  }
  ];  }
bombMap.bubbles(bombs(), {
    popupTemplate: function (geo, data) {
            return ['<div class="hoverinfo">' +  data.name,
            '<br/>Payload: ' +  data.yield + ' kilotons',
            '<br/>Country: ' +  data.country + '',
            '<br/>Date: ' +  data.date + '',
            '</div>'].join('');
    }
});
setInterval(function() {
	bombMap.bubbles(bombs(), {
	    popupTemplate: function (geo, data) {
	            return ['<div class="hoverinfo">' +  data.name,
	            '<br/>Payload: ' +  data.yield + ' kilotons',
	            '<br/>Country: ' +  data.country + '',
	            '<br/>Date: ' +  data.date + '',
	            '</div>'].join('');
	    }
	});
}, 5000);
*)
module GeoGlobals = 
  type Locations = 
    { country : string
      coordinates : float[] }

  let locations = 
    async { 
      let! json = Http.Request("GET", "/data/locations.json") 
      let lookup = 
        jsonParse<Locations[]> json 
        |> Array.map (fun l -> l.country, l.coordinates)
        |> Map.ofArray
      return lookup } |> Async.CreateNamedFuture "locations"

type geo =
  static member lookup (country:string) = async {
    let! locs = GeoGlobals.locations |> Async.AwaitFuture
    return defaultArg (locs.TryFind(country)) [| 0.0; 0.0 |] }

type math = 
  static member sqrt(f:float) = sqrt f
  static member pow(f:float, k) = Math.Pow(f, k)
  static member log(f:float, ?b) = match b with Some b -> Math.Log(f, b) | _ -> log f
  static member min(f1:float, f2:float) = min f1 f2
  static member max(f1:float, f2:float) = max f1 f2
  static member add(f1:float, f2:float) = f1 + f2
  static member times(f1:float, f2:float) = f1 * f2
  static member sub(f1:float, f2:float) = f1 - f2
  static member div(f1:float, f2:float) = f1 / f2

module YouDrawHelpers = 
  type keyvalue = 
    { key : obj 
      value : obj }

  // Adapted from: https://bl.ocks.org/1wheel/07d9040c3422dac16bd5be741433ff1e
  // (from the awesome work https://www.nytimes.com/interactive/2017/01/15/us/politics/you-draw-obama-legacy.html)
  [<Emit("""
    function youDrawIt(id, data, clipx, loy, hiy, eclr, dclr) {
  var clamp = function(a, b, c){ return Math.max(a, Math.min(b, c)) }
  var f = d3.f
  var sel = d3.select('#' + id).html('')
  var c = d3.conventions({
    parentSel: sel, 
    totalWidth: sel.node().offsetWidth, 
    height: 400, 
    margin: {left: 50, right: 50, top: 30, bottom: 30}
  })
  c.svg.append('rect').at({width: c.width, height: c.height, opacity: 0})  
  
  var lox = Number.MAX_VALUE;
  var hix = Number.MIN_VALUE;
  var cnext = Number.MAX_VALUE;
  data.forEach(function(kv) { 
    if (kv.key > clipx) cnext = Math.min(cnext, kv.key);
    lox = Math.min(lox, kv.key); hix = Math.max(hix, kv.key); });
  c.x.domain([lox, hix]);
  c.y.domain([loy, hiy]);
  c.xAxis.ticks(4).tickFormat(f())
  c.yAxis.ticks(5).tickFormat(f())
  c.drawAxis()

  var area = d3.area().x(f('key', c.x)).y0(f('value', c.y)).y1(c.height)
  var line = d3.area().x(f('key', c.x)).y(f('value', c.y))

  var clipRect = c.svg.append('clipPath#clip').append('rect')
    .at({width: c.x(clipx) - 2, height: c.height});
    var correctSel = c.svg.append('g').attr('clip-path', 'url(#clip)');

  correctSel.append('path.area').at({d: area(data)})
  correctSel.append('path.line').at({d: line(data)})
  correctSel.append('path.line').attr("stroke", eclr).attr("stroke-width", 3).at({d: line(data)})
  var yourDataSel = c.svg.append('path.your-line').attr("stroke", dclr).attr("stroke-width", 3).attr("stroke-dasharray", "5 5")

  var yourData = data
    .map(function(d){ return {key: d.key, value: d.value, defined: 0} })
    .filter(function(d){
      if (d.key == clipx) d.defined = true
      return d.key >= clipx
    });

  var completed = false
  var drag = d3.drag()
    .on('drag', function(){
      var pos = d3.mouse(this)
      var key = clamp(cnext, hix, c.x.invert(pos[0]))
      var value = clamp(0, c.y.domain()[1], c.y.invert(pos[1]))

      yourData.forEach(function(d){
        if (Math.abs(d.key - key) < .5){
          d.value = value
          d.defined = true
        }
      })

      yourDataSel.at({d: line.defined(f('defined'))(yourData)})

      if (!completed && d3.mean(yourData, f('defined')) == 1){
        completed = true
        clipRect.transition().duration(1000).attr('width', c.x(hix))
      }
    })
  c.svg.call(drag)

    }    
    youDrawIt($0, $1, $2, $3, $4, $5, $6)""")>]
  let run (id:string) (data:keyvalue[]) (clipx:int) (loy:float) (hiy:float) (eclr:string) (dclr:string) : unit = 
    failwith "JS"

type youdraw = 
  { data : series<int, float> 
    clip : int option
    min : float option
    max : float option 
    lineColor : string option
    drawColor : string option }
  static member create(data:series<int, float>) =
    { youdraw.data = data
      clip = None; min = None; max = None 
      lineColor = None; drawColor = None }
  member y.setRange(min, max) = { y with min = Some min; max = Some max }
  member y.setClip(clip) = { y with clip = Some clip }
  member y.setColors(line, draw) = { y with lineColor = Some line; drawColor = Some draw }
  member y.show(outputId) =   
    async { 
      let id = "el" + (Guid.NewGuid().ToString().Replace("-", ""))
      h?div ["class" => "youdraw"] [ h?div ["id" => id] [] ] |> renderTo (document.getElementById outputId)
      let! data = y.data.data |> Async.AwaitFuture 
      do 
        try
          let loy = match y.min with Some v -> v | _ -> data |> Seq.map snd |> Seq.min
          let hiy = match y.max with Some v -> v | _ -> data |> Seq.map snd |> Seq.max
          let clipx = match y.clip with Some v -> v | _ -> fst (data.[data.Length / 2])
          let data = data |> Array.map (fun (k, v) -> { YouDrawHelpers.keyvalue.key = k; YouDrawHelpers.keyvalue.value = v})
          let lc, dc = defaultArg y.lineColor "#606060", defaultArg y.drawColor "#FFC700"
          YouDrawHelpers.run id data clipx loy hiy lc dc
        with e ->
          Log.exn("runtime", "YouDraw failed", e) } |> Async.StartImmediate  

type timeline<'k,'v> =
  { data : series<'k,'v> 
    colors : string[]
    titleTemplate : string
    defaultFill : string
    delay : int
    overflowDelay : int
    infoSelector : 'v -> string
    locSelector : 'v -> Async<int[]>
    sizeSelector : 'v -> float
    detailsSelector : option<'v -> obj[]>
    timeSelector : 'v -> int }

  static member create(data:series<_, _>) =
    { timeline.data = data 
      colors = [| "red" |]
      defaultFill = "blue"
      delay = 750
      detailsSelector = None
      overflowDelay = 2000
      titleTemplate = "%title"
      infoSelector = fun _ -> ""
      timeSelector = fun _ -> 0 
      sizeSelector = fun _ -> 10.0
      locSelector = fun _ -> failwith "!" }

  member t.set(?fill, ?colors, ?title, ?delay, ?overflowDelay, ?details) = 
    { t with 
        colors = defaultArg colors t.colors; defaultFill = defaultArg fill t.defaultFill 
        titleTemplate = defaultArg title t.titleTemplate; delay = defaultArg delay t.delay
        detailsSelector = match details with Some d -> Some d | _ -> t.detailsSelector
        overflowDelay = defaultArg overflowDelay t.overflowDelay  }

  member t.using(coordinates, time, size, info) = 
    { t with 
        locSelector = coordinates; timeSelector = time; sizeSelector = size 
        infoSelector = info }

  member t.show(outputId) =
    let id = "map" + DateTime.Now.Ticks.ToString()
    h?div ["class" => "map"] [
      h?div ["id" => id + "_title"] [text ""]
      h?div ["id" => id; "class" => "mapcontainer" ] [] 
      h?div [] [
        h?div ["class" => "buttons"] [ h?a ["id" => id + "_btn"] [ h?i ["class" => "fa fa-pause"] [] ] ]
        h?input ["id" => id + "_player"; "type" => "range"] []
      ]
    ] |> renderTo (document.getElementById(outputId))

    let fills = t.colors |> Array.mapi (fun i c -> sprintf "item%d" i, box c)
    let map = 
      { element = document.getElementById(id)
        scope = "world"
        geographyConfig = { popupOnHover = false;  highlightOnHover = false }
        fills = JsInterop.createObj (("defaultFill", box t.defaultFill)::(List.ofArray fills))
        data = JsInterop.createObj [] }
      |> JsDatamap.create

    let objects (data:_[]) infos time = 
      let res = ResizeArray<_>()
      for i in 0 .. data.Length - 1 do
        let color, (loc:_[]), v, ct = data.[i]
        if ct = time then
          ( match t.detailsSelector with 
            | Some os -> [ "details", os v |> Seq.map string |> String.concat "" |> box ] 
            | _ -> [] ) @
          [ "radius", box (t.sizeSelector v)
            "borderWidth", box "1px"
            "fillKey", box (sprintf "item%d" (color % fills.Length))
            "info", box (defaultArg (Map.tryFind (sprintf "%O, %O" loc.[0] loc.[1]) infos) "")
            "latitude", box loc.[0]
            "longitude", box loc.[1] ]
          |> JsInterop.createObj |> res.Add
      res.ToArray()

    async { 
      // Get data and calculate locations
      let! data = t.data.data |> Async.AwaitFuture
      let locs = Array.zeroCreate data.Length
      for i in 0 .. data.Length - 1 do
        let! loc = t.locSelector (snd data.[i])
        locs.[i] <- loc

      // Coloring keys based on locations
      let colorLookup = locs |> Seq.distinct |> Seq.mapi (fun i l -> List.ofArray l, i) |> Map.ofSeq
      let data = Array.map2 (fun (_, v) locs -> colorLookup.[List.ofArray locs], locs, v, t.timeSelector v) data locs      
      let infosLookup = data |> Seq.groupBy (fun (_, loc, _, _) -> sprintf "%O, %O" loc.[0] loc.[1]) |> Seq.map (fun (loc, vals) ->  
        loc, vals |> Seq.map (fun (_, _, v, _) -> t.infoSelector v) |> Seq.distinct |> String.concat "<br />") |> Map.ofSeq
      
      // Time range
      let times = data |> Array.map (fun  (_, _, _, t) -> t) |> Seq.distinct |> Seq.sort |> Array.ofSeq
      let lo, hi = Seq.min times, Seq.max times

      let player = document.getElementById(id + "_player") :?> HTMLInputElement
      let btn = document.getElementById(id + "_btn") :?> HTMLAnchorElement
      if times.Length = 1 then 
        player.style.display <- "none"
        btn.style.display <- "none"
      player.min <- string 0
      player.value <- string 0
      player.max <- string (times.Length - 1)
      let render () =
        let y = times.[int player.value]
        let o = objects data infosLookup y
        h?h2 [] [text (t.titleTemplate.Replace("%title", string y))] |> renderTo (document.getElementById(id + "_title"))
        let config = 
          { key = System.Func<_, _>(fun data -> jsonStringify [| getProp data "latitude"; getProp data "longitude" |])
            popupTemplate = System.Func<_, _, _>(fun geo data -> 
              match t.detailsSelector with
              | None -> sprintf "<div style='pointer-events:none' class='hoverinfo'>%s</div>" (getProp data "info")
              | Some _ -> sprintf "<div style='pointer-events:none' class='hoverinfo'><strong>%s</strong><br /> %s </div>" (getProp data "info") (getProp data "details")) }
        map.bubbles(o, config) 

      let mutable autoPlay = true
      let startPlay () = async {
        while autoPlay do
          let value = int player.value
          render()
          player.value <- string (if value + 1 = times.Length then 0 else value + 1)
          do! Async.Sleep(if value + 1 = times.Length then t.overflowDelay else t.delay) } |> Async.StartImmediate
      
      player.onchange <- fun e ->
        autoPlay <- false
        h?i ["class" => "fa fa-play"] [] |> renderTo btn
        render() |> box
      player.oninput <- player.onchange

      btn.onclick <- fun e ->
        autoPlay <- not autoPlay
        h?i ["class" => if autoPlay then "fa fa-pause" else "fa fa-play"] [] |> renderTo btn
        if autoPlay then startPlay()
        box ()

      startPlay() } |> Async.StartImmediate

(*


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

    
    *)