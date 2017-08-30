namespace TheGamma.Interactive

open Fable.Core
open Fable.Helpers
open Fable.Import.Browser

open TheGamma.Common
open TheGamma.Series
open TheGamma.Html
open TheGamma.Interactive.Compost
open TheGamma.Interactive.Compost.Derived

module FsOption = Microsoft.FSharp.Core.Option

// ------------------------------------------------------------------------------------------------
// Chart options
// ------------------------------------------------------------------------------------------------

type AxisOptions = 
  { minValue : obj option
    maxValue : obj option 
    label : string option
    labelOffset : float option 
    labelMinimalSize : float option }
  static member Default = { minValue = None; maxValue = None; label = None; labelOffset = None; labelMinimalSize = None }

type LegendOptions = 
  { position : string }
  static member Default = { position = "none" }

type ChartOptions =
  { size : float option * float option 
    xAxis : AxisOptions
    yAxis : AxisOptions 
    title : string option
    legend : LegendOptions
    emptyMessage : string option } 
  static member Default = 
    { size = None, None; title = None
      legend = LegendOptions.Default
      xAxis = AxisOptions.Default
      yAxis = AxisOptions.Default
      emptyMessage = None }

// ------------------------------------------------------------------------------------------------
// Rendering helpers
// ------------------------------------------------------------------------------------------------

module InteractiveHelpers =
  let showAppAsync outputId opts (data:series<_, _>) initial render update = async { 
    let id = "container" + System.Guid.NewGuid().ToString().Replace("-", "")
    h?div ["id" => id] [ ] |> renderTo (document.getElementById(outputId))        

    // Get data & wait until the element is created
    let! data = data.data |> Async.AwaitFuture 
    let mutable i = 10
    while i > 0 && document.getElementById(id) = null do
      do! Async.Sleep(10)
      i <- i - 1
    let element = document.getElementById(id)
    if data.Length = 0 && opts.emptyMessage.IsSome then
      h?p ["class"=>"placeholder"] [ text opts.emptyMessage.Value ] |> renderTo element
    else
      let size = 
        ( match opts.size with Some w, _ -> w | _ -> element.clientWidth ),
        ( match opts.size with _, Some h -> h | _ -> max 400. (element.clientWidth / 2.) ) 
      do
        try
          createVirtualDomApp outputId (initial data) (render data size) (update data)
        with e ->
          Log.exn("GUI", "Interactive rendering failed: %O", e) } 

  let showApp outputId opts data initial render update = 
    showAppAsync outputId opts data initial render update |> Async.StartImmediate 

  let showStaticAppAsync outputId opts data render = 
    showAppAsync outputId opts data
      (fun _ -> ())
      (fun _ size _ _ -> render size)
      (fun _ _ _ -> ())

  let showStaticApp outputId opts data render = 
    showApp outputId opts data
      (fun _ -> ())
      (fun data size _ _ -> render data size)
      (fun _ _ _ -> ())

  let calclateMax maxValue data = 
    let max = match maxValue with Some m -> m | _ -> Seq.max (Seq.map snd data)
    snd (Scales.adjustRange (0.0, max))

  let createLogger id logger = 
    match logger with
    | Some log -> fun event data -> 
        JsInterop.createObj [
          "event", box event
          "id", box id
          "data", JsInterop.createObj data
        ] |> log
    | None -> fun _ _ -> () 

module CompostHelpers = 
  let (|Cont|) = function COV(CO x) -> x | _ -> failwith "Expected continuous value"
  let (|Cat|) = function CAR(CA x, r) -> x, r | _ -> failwith "Expected categorical value"
  let Cont x = COV(CO x)
  let Cat(x, r) = CAR(CA x, r)
  let orElse (a:option<_>) b = if a.IsSome then a else b
  let vega10 = [| "#1f77b4"; "#ff7f0e"; "#2ca02c"; "#d62728"; "#9467bd"; "#8c564b"; "#e377c2"; "#7f7f7f"; "#bcbd22"; "#17becf" |]
  let infinitely s = 
    if Seq.isEmpty s then seq { while true do yield "black" }
    else seq { while true do yield! s }

open CompostHelpers

// ------------------------------------------------------------------------------------------------
// Ordinary charts
// ------------------------------------------------------------------------------------------------

module Internal = 

  let prevAndNext (data:_[]) = 
    if data.Length = 1 then
      [| data.[0], data.[0], data.[0] |]
    else 
      let res = ResizeArray<_> [| data.[0], data.[0], data.[1] |]
      for x in 1 .. data.Length - 2 do res.Add (data.[x-1], data.[x], data.[x+1])
      res.Add(data.[data.Length - 2], data.[data.Length - 1], data.[data.Length - 1])
      res.ToArray()

  let inline interpolate (x1, y1) (x2, y2) x = 
    if x1 = x2 then [||] else 
    [| x, y1 + (y2 - y1) * (x - x1) / (x2 - x1) |]

  let inline interpolateAt x points = 
    let p, n = points |> Seq.pairwise |> Seq.find (fun ((p, _), (n, _)) -> p <= x && n >= x)
    interpolate p n x |> Seq.head

  let inline cropLine points lx hx = 
    let inRange = 
      points 
      |> prevAndNext 
      |> Array.filter (fun (p, (x, y), n) -> x > lx && x < hx)
    if inRange.Length = 0 then [||] else
    let (s1, s2, _), (_, e1, e2) = inRange.[0], inRange.[inRange.Length-1]
    let core = inRange |> Array.map (fun (_, v, _) -> v)
    Array.concat [interpolate s1 s2 lx; core; interpolate e1 e2 hx]

  // Helpers
  let arrayMap f s = Array.map f s // REVIEW: Hack to avoid Float64Array (which behaves oddly in Safari) see: https://github.com/zloirock/core-js/issues/285

  let parseColor (clr:string) = 
    let r = parseInt (clr.Substring(1,2)) 16
    let g = parseInt (clr.Substring(3,2)) 16
    let b = parseInt (clr.Substring(5,2)) 16
    float r, float g, float b

  let formatColor (r, g, b) = 
    let fi n = (formatInt (int n) 16).PadLeft(2, '0')
    "#" + fi r + fi g + fi b

  let mapColor f (clr:string) = 
    formatColor (f (parseColor clr))

  type ScalePoints = 
    { Minimum : Value<1>; Maximum : Value<1>; Middle : Value<1>; PixelSize : float }

  type ChartContext = 
    { Chart : Shape<1, 1> 
      Width : float
      Height : float
      XPoints : ScalePoints 
      YPoints : ScalePoints
      XData : obj[]
      YData : obj[]
      Padding : float * float * float * float
      ChartOptions : ChartOptions }

  let calculateScales (width, height) chart = 
    let (Scales.Scaled((sx, sy), _, _)) as scaled = Scales.calculateScales Compost.defstyle chart
    let getPoints = function
      | Continuous(CO lo, CO hi) as scale -> 
        // Calculate projection and calculate pixel size
        let master = Projections.Scale((0.0, width), (height, 0.0))
        let (Projections.Projected(projection, _, _)) = Projections.calculateProjections scaled master
        let (Cont z, _) = Projections.projectInv (scale, scale) (CO 100.0, CO 0.0) projection
        let (Cont o, _) = Projections.projectInv (scale, scale) (CO 101.0, CO 0.0) projection
        { Minimum = COV(CO lo); Maximum = COV(CO hi); Middle = COV(CO ((hi + lo) / 2.)); PixelSize = abs (z - o) }
      | Categorical(vals) -> 
        { Minimum = CAR(vals.[0], 0.0); Maximum = CAR(vals.[vals.Length-1], 1.0)
          Middle = if vals.Length % 2 = 1 then CAR(vals.[vals.Length/2], 0.5)
                   else CAR(vals.[vals.Length/2], 0.0)
          PixelSize = 0.0 } // TODO: This should probably not be allowed
    getPoints sx, getPoints sy

  let initChart size xdata ydata options chart =
    let px, py = calculateScales size chart 
    { Chart = chart; XPoints = px; YPoints = py; Padding = (20., 20., 20., 20.)
      XData = xdata; YData = ydata; ChartOptions = options 
      Width = fst size; Height = snd size }

  let applyStyle f chart = 
    Style(f, chart)

  let applyStyleCtx f ctx = 
    { ctx with Chart = Style(f, ctx.Chart) }

  let applyInteractive e chart = 
    Interactive(e, chart)

  let applyInteractiveCtx e ctx = 
    { ctx with Chart = Interactive(e ctx, ctx.Chart) }

  /// Add InnerScale (when scales are set explicitly) and AutoScale for the rest
  /// Recalculate points after changing the scales to make sure they're up to date
  let applyScales ctx = 
    let getInnerScale axis sp = 
      match sp with
      | _ when axis.minValue = None && axis.maxValue = None -> None 
      | { Minimum = COV(CO lo); Maximum = COV(CO hi) } ->
          let amin = FSharp.Core.Option.map dateOrNumberAsNumber axis.minValue
          let amax = FSharp.Core.Option.map dateOrNumberAsNumber axis.maxValue
          let lo, hi = defaultArg amin lo, defaultArg amax hi
          Some(CO(unbox lo), CO(unbox hi))
      | _ -> None
    let sx = getInnerScale ctx.ChartOptions.xAxis ctx.XPoints
    let sy = getInnerScale ctx.ChartOptions.yAxis ctx.YPoints
    let chart = AutoScale(sx.IsNone, sy.IsNone, InnerScale(sx, sy, ctx.Chart))
    let xp, yp = calculateScales (ctx.Width, ctx.Height) chart 
    { ctx with Chart = chart; XPoints = xp; YPoints = yp }

  /// 
  let applyAxes tlab rlab blab llab ctx = 
    let style data =
      let isDate = data |> Seq.exists isDate
      if isDate then
        let values = data |> arrayMap dateOrNumberAsNumber
        let lo, hi = asDate(Seq.min values), asDate(Seq.max values)
        if (hi - lo).TotalDays <= 1. then fun _ (Cont v) -> formatTime(asDate(v))
        else fun _ (Cont v) -> formatDate(asDate(v))
      else Compost.defaultFormat
    let chart =
      Axes(tlab, rlab, blab, llab, ctx.Chart) |> applyStyle (fun s -> 
        { s with FormatAxisXLabel = style ctx.XData; FormatAxisYLabel = style ctx.YData })
    { ctx with Chart = Padding(ctx.Padding, chart) }    


  let applyLegend (width, height) labels ctx =     
    let labels = Array.ofSeq labels

    match ctx.ChartOptions.legend.position, width > 600. with
    | "right", _ | "auto", true -> 
        let ptop, _, _, _ = ctx.Padding
        let labs = 
          InnerScale(Some(CO 0., CO 100.), None, 
              Layered
                [ for clr, lbl in labels do
                    let style clr = applyStyle (fun s -> { s with Font = "9pt Roboto,sans-serif"; Fill=Solid(1., HTML clr); StrokeColor=(0.0, RGB(0,0,0)) })
                    yield Padding((4., 0., 4., 0.), Bar(CO 6., CA lbl)) |> style clr
                    yield Text(COV(CO 8.), CAR(CA lbl, 0.5), VerticalAlign.Middle, HorizontalAlign.Start, 0., lbl) |> style "black"
                ] ) 
        let lwid, lhgt = (width - 250.) / width, (ptop + float labels.Length * 30.) / height    
        let chart = 
          Layered
            [ OuterScale(Some(Continuous(CO 0.0, CO lwid)), Some(Continuous(CO 0.0, CO 1.0)), ctx.Chart)
              OuterScale(Some(Continuous(CO lwid, CO 1.0)), Some(Continuous(CO 0.0, CO lhgt)), Padding((ptop, 0., 0., 20.), labs)) ]
        { ctx with Chart = chart }

    | "bottom", _ | "auto", false -> 
        let _, pright, _, pleft = ctx.Padding
        let labs = 
          InnerScale(Some(CO 0., CO 100.), None, 
              Layered
                [ for clr, lbl in labels do
                    let style clr = applyStyle (fun s -> { s with Font = "9pt Roboto,sans-serif"; Fill=Solid(1., HTML clr); StrokeColor=(0.0, RGB(0,0,0)) })
                    yield Padding((4., 0., 4., 0.), Shape[ COV(CO 94.), CAR(CA lbl, 0.); COV(CO 94.), CAR(CA lbl, 1.); COV(CO 100.), CAR(CA lbl, 1.); COV(CO 100.), CAR(CA lbl, 0.) ]) |> style clr
                    yield Text(COV(CO 92.), CAR(CA lbl, 0.5), VerticalAlign.Middle, HorizontalAlign.End, 0., lbl) |> style "black"
                ] ) 
        let lhgt = (height - float labels.Length * 30.) / height
        let chart = 
          Layered
            [ OuterScale(Some(Continuous(CO 0.0, CO 1.0)), Some(Continuous(CO 0.0, CO lhgt)), ctx.Chart)
              OuterScale(Some(Continuous(CO 0.0, CO 1.0)), Some(Continuous(CO lhgt, CO 1.0)), Padding((20., pright, 0., pleft), labs)) ]
        { ctx with Chart = chart }
    | _ -> ctx


  let applyLabels ctx =     
    let ptop, pright, pbot, pleft = ctx.Padding
    let lblStyle font chart = 
      chart |> applyStyle (fun s -> 
        { s with StrokeWidth = Pixels 0; Fill = Solid(1., HTML "black"); Font = font })

    // X axis label, Y axis label
    let chart = ctx.Chart
    let chart, pbot = 
      match ctx.ChartOptions.xAxis.label, ctx.ChartOptions.xAxis.labelMinimalSize with
      | _, Some min when ctx.Height < min -> chart, pbot
      | Some xl, _ -> 
          let offs = defaultArg ctx.ChartOptions.xAxis.labelOffset 40.
          let lbl = Offset((0., offs), Text(ctx.XPoints.Middle, ctx.YPoints.Minimum, VerticalAlign.Middle, HorizontalAlign.Center, 0.0, xl))
          Layered [ chart; lblStyle "bold 9pt Roboto,sans-serif" lbl ], (offs + 10.) - 30. // +10 space for label, -30 because offset 30 is added by Axes
      | _ -> chart, pbot
    let chart, pleft = 
      match ctx.ChartOptions.yAxis.label, ctx.ChartOptions.yAxis.labelMinimalSize with
      | _, Some min when ctx.Width < min -> chart, pbot
      | Some yl, _ -> 
          let offs = defaultArg ctx.ChartOptions.yAxis.labelOffset 60.
          let lbl = Offset((-offs, 0.), Text(ctx.XPoints.Minimum, ctx.YPoints.Middle, VerticalAlign.Middle, HorizontalAlign.Center, -90.0, yl))
          Layered [ chart; lblStyle "bold 9pt Roboto,sans-serif" lbl ], (offs + 10.) - 50. // +10 space for label, -50 because offset 50 is added by Axes
      | _ -> chart, pleft

    // Chart title
    let chart, ptop = 
      match ctx.ChartOptions.title with 
      | Some title ->
          let ttl = Offset((0., -30.), Text(ctx.XPoints.Middle, ctx.YPoints.Maximum, VerticalAlign.Hanging, HorizontalAlign.Center, 0.0, title))
          Layered [ chart; lblStyle "13pt Roboto,sans-serif" ttl ], 40.
      | _ -> chart, ptop

    { ctx with Chart = chart; Padding = (ptop, pright, pbot, pleft) }

  let createChart size ctx =   
    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg false false size ctx.Chart
    ]
        
  let inAxis axis value =
    if axis.minValue.IsSome && dateOrNumberAsNumber value < dateOrNumberAsNumber axis.minValue.Value then false
    elif axis.maxValue.IsSome && dateOrNumberAsNumber value > dateOrNumberAsNumber axis.maxValue.Value then false
    else true
                
module Charts = 
  open Internal

  // Charts
  
  let renderBubbles trigger selectedBubble chartOptions size bc (data:(obj * obj * obj option * obj option)[]) =   
    let xdata, ydata = Array.map (fun (x, _, _, _) -> x) data, Array.map (fun (_, y, _, _) -> y) data    
    let tryFindBubble rx ry px py = 
      data 
      |> Seq.tryFindIndex (fun (x, y, s, _) ->
        let size = unbox (defaultArg s (box 2.))
        let rx, ry = rx * size * 2., ry * size * 2.
        let x, y = dateOrNumberAsNumber x, dateOrNumberAsNumber y
        ((px - x)*(px - x))/(rx*rx) + ((py - y)*(py - y))/(ry*ry) < 1.0 )
    Layered [
      for i, (x, y, s, l) in Seq.mapi (fun i v -> i, v) data do
        if inAxis chartOptions.xAxis x && inAxis chartOptions.yAxis y then
          let size = unbox (defaultArg s (box 2.))
          let b = Bubble(COV(CO (dateOrNumberAsNumber x)), COV(CO (dateOrNumberAsNumber y)), size, size) 
          let alpha = if i = selectedBubble then 1.0 else 0.6
          yield b |> applyStyle (fun s -> { s with StrokeWidth = Pixels 0; Fill = Solid(alpha, HTML bc) }) 
      for i, (x, y, s, l) in Seq.mapi (fun i v -> i, v) data do
        if i = selectedBubble && l.IsSome then
          let t = Text(COV(CO (dateOrNumberAsNumber x)), COV(CO (dateOrNumberAsNumber y)), VerticalAlign.Baseline, HorizontalAlign.Center, 0., unbox l.Value)
          yield t |> applyStyle (fun s -> { s with Font = "10pt roboto,sans-serif"; StrokeWidth = Pixels 0; Fill = Solid(1., HTML "black") }) ]
    |> initChart size xdata ydata chartOptions
    |> applyInteractiveCtx (fun ctx -> 
        [ TouchStart(fun evt (Cont x, Cont y) -> 
            let bub = defaultArg (tryFindBubble ctx.XPoints.PixelSize ctx.YPoints.PixelSize x y) -1
            if bub <> selectedBubble then trigger bub )
          MouseMove(fun evt (Cont x, Cont y) -> 
            let bub = defaultArg (tryFindBubble ctx.XPoints.PixelSize ctx.YPoints.PixelSize x y) -1
            if bub <> selectedBubble then trigger bub)
          MouseLeave(fun _ -> trigger -1) ])
    |> applyScales 
    |> applyLabels 
    |> applyAxes false false true true
    //|> applyLegend chartOptions
    |> createChart size 

  let renderLines isArea chartOptions size lcs labels (data:(obj * obj)[][]) =   
    let xdata, ydata = Array.collect (Array.map fst) data, Array.collect (Array.map snd) data    
    Layered [
      for clr, line in Seq.zip (infinitely lcs) data do
        let points = 
          [ for x, y in line do
              if inAxis chartOptions.xAxis x && inAxis chartOptions.yAxis y then
                yield COV(CO (dateOrNumberAsNumber x)), COV(CO (dateOrNumberAsNumber y)) ]
        if not (List.isEmpty points) then 
          if isArea then yield Area points |> applyStyle (fun s -> { s with Fill = Solid(0.4, HTML clr); StrokeWidth = Pixels 0  })
          yield Line points |> applyStyle (fun s -> { s with StrokeColor = 1.0, HTML clr; StrokeWidth = Pixels 2  }) ]
    |> initChart size xdata ydata chartOptions
    |> applyScales 
    |> applyLabels
    |> applyAxes false false true true    
    |> applyLegend size (Seq.zip (infinitely lcs) labels)
    |> createChart size 

  let renderColsBars isBar inlineLabels chartOptions size clrs labels (data:(string * float)[]) =   
    let xdata, ydata = 
      if isBar then Array.map (snd >> box) data, Array.map (fst >> box) data    
      else Array.map (fst >> box) data, Array.map (snd >> box) data    
    
    let { XPoints = xp; YPoints = yp } =
      Layered [ for lbl, v in data -> if isBar then Bar(CO v, CA lbl) else Column(CA lbl, CO v) ] 
      |> initChart size xdata ydata chartOptions |> applyScales 
    
    let chartOptions = 
      if isBar && inlineLabels && chartOptions.yAxis.labelOffset.IsNone then { chartOptions with yAxis = { chartOptions.yAxis with labelOffset = None } }
      elif not isBar && inlineLabels && chartOptions.yAxis.labelOffset.IsNone then { chartOptions with xAxis = { chartOptions.xAxis with labelOffset = None } }
      else chartOptions

    Layered [
      for clr, (lbl, v) in Seq.zip (infinitely clrs) data do
        let elem = 
          if isBar then Padding((6.,0.,6.,1.), Bar(CO v, CA lbl))
          else Padding((0.,6.,1.,6.), Column(CA lbl, CO v)) 
        let label = 
          if not inlineLabels then None
          elif isBar then Some(Offset((20., 0.), Text(xp.Minimum, CAR(CA lbl, 0.5), VerticalAlign.Middle, HorizontalAlign.Start, 0.0, lbl)))
          else Some(Offset((0., -20.), Text(CAR(CA lbl, 0.5), yp.Minimum, VerticalAlign.Middle, HorizontalAlign.Start, -90.0, lbl)))
        yield elem |> applyStyle (fun s -> { s with Fill = Solid(0.6, HTML clr) }) 
        if label.IsSome then
          let clr = clr |> mapColor (fun (r,g,b) -> r*0.8, g*0.8, b*0.8)
          yield label.Value |> applyStyle (fun s -> { s with Font = "11pt Roboto,sans-serif"; Fill = Solid(1.0, HTML clr); StrokeWidth = Pixels 0 }) ]
    |> initChart size xdata ydata chartOptions
    |> applyScales 
    |> applyLabels
    |> applyAxes false false (not (inlineLabels && not isBar)) (not (inlineLabels && isBar))
    |> applyLegend size (Seq.zip (infinitely clrs) labels)
    |> createChart size 

// ------------------------------------------------------------------------------------------------
// You Guess Line
// ------------------------------------------------------------------------------------------------

module YouGuessLineHelpers = 
  open Internal

  type YouDrawEvent = 
    | ShowResults
    | Draw of float * float

  type Status = 
    | Autofilled of float
    | Set of float
    | NotSet
    
  type YouDrawState = 
    { Completed : bool
      Clip : float
      Data : (float * float)[]
      XData : obj[]
      YData : obj[]
      Guessed : (float * Status)[] 
      IsKeyDate : bool }

  let initState completed data clipx = 
    let isDate = data |> Seq.exists (fst >> isDate)
    let numData = data |> Array.map (fun (k, v) -> dateOrNumberAsNumber k, v)
    { Completed = completed
      Data = numData
      XData = Array.map (fst >> box) data
      YData = Array.map (snd >> box) data
      Clip = clipx
      IsKeyDate = isDate
      Guessed = 
        [| for x, y in numData do 
            if x > clipx then 
              if completed then yield x, Set y 
              else yield x, NotSet |] }

  let handler log state evt = 
    let collectData () = state.Data |> Array.map (fun (k, v) -> [| box k; box v |]) |> box
    let collectGuesses () = state.Guessed |> Seq.choose (function (k, Set v) -> Some [| box k; box v |] | _ -> None) |> Array.ofSeq |> box
    match evt with
    | ShowResults -> 
        log "completed" [ "guess", collectGuesses(); "values", collectData() ]
        { state with Completed = true }
    | Draw (downX, downY) ->
        let indexed = Array.indexed state.Guessed
        let nearest, _ = indexed |> Array.minBy (fun (_, (x, _)) -> abs (downX - x))
        { state with
            Guessed = indexed |> Array.map (fun (i, (x, y)) -> 
              match y with
              | _ when i = nearest -> (x, Set downY) 
              | NotSet when i > nearest -> (x, Autofilled downY)
              | _ -> (x, y) ) }

  let render (solvedComment:string option) interactive chartOptions (width, height) (markers:(float*obj)[]) (leftLbl, rightLbl) 
    (leftClr,rightClr,guessClr,markClr) trigger state = 

    let all = 
      [| for x, y in state.Data -> Cont x, Cont y |]
    let known = 
      [| for x, y in state.Data do if x <= state.Clip then yield Cont x, Cont y |]
    let right = 
      [| yield Array.last known
         for x, y in state.Data do if x > state.Clip then yield Cont x, Cont y |]
    let guessed = 
      [| yield Array.last known
         for x, y in state.Guessed do match y with Set v | Autofilled v -> yield Cont x, Cont v | _ -> () |]


    let loy = match chartOptions.yAxis.minValue with Some v -> unbox v | _ -> state.Data |> Seq.map snd |> Seq.min
    let hiy = match chartOptions.yAxis.maxValue with Some v -> unbox v | _ -> state.Data |> Seq.map snd |> Seq.max       
    let lx, ly = (fst (Seq.head state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let rx, ry = (fst (Seq.last state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let tx, ty = float state.Clip, hiy - (hiy - loy) / 10.
    let setColor c s = { s with Font = "12pt Roboto,sans-serif"; Fill=Solid(1.0, HTML c); StrokeColor=(0.0, RGB(0,0,0)) }
    let labels = 
      Shape.Layered [
        Style(setColor leftClr, Shape.Text(COV(CO lx), COV(CO ly), VerticalAlign.Baseline, HorizontalAlign.Center, 0., leftLbl))
        Style(setColor rightClr, Shape.Text(COV(CO rx), COV(CO ry), VerticalAlign.Baseline, HorizontalAlign.Center, 0., rightLbl))
      ]

    let LineStyle shape = 
      Style((fun s -> 
        { s with 
            Fill = Solid(1.0, HTML "transparent"); 
            StrokeWidth = Pixels 2; 
            StrokeDashArray = [Integer 5; Integer 5]
            StrokeColor=0.6, HTML markClr }), shape)
    let FontStyle shape = 
      Style((fun s -> { s with Font = "11pt Roboto,sans-serif"; Fill = Solid(1.0, HTML markClr); StrokeColor = 0.0, HTML "transparent" }), shape)
    
    let loln, hiln = Scales.adjustRange (loy, hiy)
    let markers = [
        for i, (x, lbl) in Seq.mapi (fun i v -> i, v) markers do
          let kl, kt = if i % 2 = 0 then 0.90, 0.95 else 0.80, 0.85
          let ytx = loln + (hiln - loln) * kt
          let hiln = loln + (hiln - loln) * kl
          yield Line [(COV(CO x), COV(CO loln)); (COV(CO x), COV(CO hiln))] |> LineStyle
          yield Text(COV(CO x), COV(CO ytx), VerticalAlign.Middle, HorizontalAlign.Center, 0., string lbl) |> FontStyle
      ]

    let coreChart = 
      Layered [
        yield labels
        yield! markers
        yield Style(Drawing.hideFill >> Drawing.hideStroke, Line all)
        yield Style(
          (fun s -> { s with StrokeColor = (1.0, HTML leftClr); Fill = Solid(0.2, HTML leftClr) }), 
          Layered [ Area known; Line known ]) 
        if state.Completed then
          yield Style((fun s -> 
            { s with 
                StrokeColor = (1.0, HTML rightClr)
                StrokeDashArray = [ Percentage 0.; Percentage 100. ]
                Fill = Solid(0.0, HTML rightClr)
                Animation = Some(1000, "ease", fun s -> 
                  { s with
                      StrokeDashArray = [ Percentage 100.; Percentage 0. ]
                      Fill = Solid(0.2, HTML rightClr) } 
                ) }), 
            Layered [ Area right; Line right ])                 
        if guessed.Length > 1 then
          yield Style(
            (fun s -> { s with StrokeColor = (1.0, HTML guessClr); StrokeDashArray = [ Integer 5; Integer 5 ] }), 
            Line guessed ) ]
    
    let { Internal.ChartContext.Chart = chart } = 
      coreChart
      |> initChart (width, height) state.XData state.YData chartOptions 
      |> applyScales 
      |> applyLabels 
      |> applyAxes false false true true

    let chart =
      chart |> applyInteractive
          ( if state.Completed then []
            else
              [ MouseMove(fun evt (Cont x, Cont y) -> 
                  if (int evt.buttons) &&& 1 = 1 then trigger(Draw(x, y)) )
                TouchMove(fun evt (Cont x, Cont y) -> 
                  trigger(Draw(x, y)) )
                MouseDown(fun evt (Cont x, Cont y) -> trigger(Draw(x, y)) )
                TouchStart(fun evt (Cont x, Cont y) -> trigger(Draw(x, y)) ) ])
    
    h?div ["style"=>"text-align:center;padding-top:20px"] [
      yield Compost.createSvg false false (width, height) chart
      if state.Completed && solvedComment.IsSome then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?p ["class"=>"solved"] [text solvedComment.Value]
        ]
      elif interactive then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?button [
              yield "type" => "button"
              yield "click" =!> fun _ _ -> trigger ShowResults
              if state.Guessed |> Seq.last |> snd = NotSet then
                yield "disabled" => "disabled"
            ] [ text "Show me how I did" ]
          ]
    ]

// ------------------------------------------------------------------------------------------------
// You Guess Line offset
// ------------------------------------------------------------------------------------------------

module YouGeussLineOffsetHelpers = 
  open Internal

  type YouDrawEvent =   
    | Animate
    | HideOverlay
    | ShowResults
    | StopMoving
    | StartMoving of float
    | ContinueMoving of float

  type YouDrawState = 
    { Completed : bool
      Interactive : bool
      Data : (float * float)[]
      XData : obj[]
      YData : obj[]
      IsKeyDate : bool
      OffsetStart : float option
      Offset : float 
      OffsetRange : float * float
      XRange : float * float
      YRange : float * float 
      OverlayHiding : bool
      OverlayOpacity : float
      CompletionStep : float
      ChartOptions : ChartOptions }

  let initState completed chartOptions data = 
    let isDate = data |> Seq.exists (fst >> isDate)
    let numData = data |> Array.map (fun (k, v) -> dateOrNumberAsNumber k, v)

    let range axis data = 
      dateOrNumberAsNumber (defaultArg axis.minValue (box (Seq.min data))),
      dateOrNumberAsNumber (defaultArg axis.maxValue (box (Seq.max data)))
    let lx, hx = range chartOptions.xAxis (Seq.map snd numData)
    let ly, hy = range chartOptions.yAxis (Seq.map fst numData)
    let minData = data |> Seq.map (fst >> dateOrNumberAsNumber) |> Seq.min
    let maxData = data |> Seq.map (fst >> dateOrNumberAsNumber) |> Seq.max

    { Completed = completed
      Interactive = not completed
      ChartOptions = chartOptions
      Data = numData
      XData = Array.map (snd >> box) data
      YData = Array.map (fst >> box) data
      IsKeyDate = isDate 
      Offset = ly - minData
      OffsetRange = hy - maxData, ly - minData
      OffsetStart = None
      XRange = lx, hx
      YRange = ly, hy 
      OverlayHiding = completed
      OverlayOpacity = if completed then 1. else 0.
      CompletionStep = if completed then 1. else 0. }

  let handler log state evt = 
    let collectData () = state.Data |> Array.map (fun (k, v) -> [| box k; box v |]) |> box
    //let collectGuesses () = state.Guessed |> Seq.choose (function (k, Set v) -> Some [| box k; box v |] | _ -> None) |> Array.ofSeq |> box
    printfn "offset=%A" state.Offset
    match evt with
    | StopMoving -> { state with OffsetStart = None }
    | HideOverlay -> { state with OverlayOpacity = state.OverlayOpacity + 0.1 }
    | StartMoving y -> 
        { state with OffsetStart = Some (y - state.Offset); OverlayHiding = true }
    | ContinueMoving y -> 
        let lo, hi = state.OffsetRange
        if state.OffsetStart.IsNone then state // should not happen
        else { state with Offset = min hi (max lo (y - state.OffsetStart.Value)) }
    | ShowResults -> 
        log "completed" [ (*"guess", collectGuesses();*) "values", collectData() ]
        { state with Completed = true }
    | Animate -> { state with CompletionStep = min 1.0 (state.CompletionStep + 0.05) }

  let render (solvedComment:string option) interactive (width, height) labels (markers:(float*obj)[]) (guessClr, correctClr, markerClr) trigger state = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    if state.OverlayHiding && state.OverlayOpacity < 1. then
      window.setTimeout((fun () -> trigger HideOverlay), 50) |> ignore

    let (lx, hx), (ly, hy) = state.XRange, state.YRange
    let visiblePoints offset = 
      let lo = dateOrNumberAsNumber state.ChartOptions.yAxis.minValue.Value - offset
      let hi = dateOrNumberAsNumber state.ChartOptions.yAxis.maxValue.Value - offset
      cropLine state.Data lo hi |> Array.map (fun (y, x) -> COV(CO x), COV(CO (y + offset)))

    let coreChart = 
      Layered [
        for (y, str), label in Seq.zip markers labels do
          let _, inx = state.Data |> Seq.map (fun (y, x) -> y + state.Offset * (1.0 - state.CompletionStep), x) |> interpolateAt y
          let b = Bubble(COV(CO hx), COV(CO y), 4., 4.) 
          let t = Offset((0., 5.), Text(COV(CO hx), COV(CO y), VerticalAlign.Hanging, HorizontalAlign.End, 0.0, label))
          yield t |> applyStyle (fun s -> { s with StrokeWidth = Pixels 0; Fill = Solid(0.8, HTML markerClr) }) 
          let l = Line [COV(CO inx), COV(CO y); COV(CO hx), COV(CO y)] 
          yield l |> applyStyle (fun s -> { s with StrokeWidth = Pixels 2; StrokeColor = 0.8, HTML markerClr }) 

        if not state.Completed then
          yield Style(
            (fun s -> { s with Fill = Solid(0.5, HTML guessClr) }), 
            VShiftedArea(lx, visiblePoints state.Offset) )
        if state.Interactive then
          yield Style(
            (fun s -> { s with StrokeColor = (1.0, HTML guessClr); StrokeDashArray = [ Integer 5; Integer 5 ] }), 
            Line(visiblePoints state.Offset) )

        if state.Completed then 
          let r1, g1, b1 = parseColor guessClr
          let r2, g2, b2 = parseColor correctClr
          let dc c1 c2 = (1.0 - state.CompletionStep) * c1 + state.CompletionStep * c2
          let clr = formatColor (dc r1 r2, dc g1 g2, dc b1 b2)
          let correct = visiblePoints (state.Offset * (1.0 - state.CompletionStep))
          yield Style(
            (fun s -> { s with Fill = Solid(0.5, HTML clr) }), 
            VShiftedArea(lx, correct) )
          yield Style(
            (fun s -> { s with StrokeColor = (1.0, HTML clr) }), 
            Layered [ Line correct]) 
        
        yield 
          Shape [ 
            COV(CO lx), COV(CO ly); COV(CO lx), COV(CO hy);
            COV(CO hx), COV(CO hy); COV(CO hx), COV(CO ly) ]
          |> applyStyle (fun x -> { x with Fill = Solid(0.75 * (1. - state.OverlayOpacity), HTML "#eaeaea") })

      ] |> applyStyle(fun s -> 
        let cursor = 
          if state.Completed then ""
          //elif state.OffsetStart.IsSome then "move,grabbing,-moz-grabbing,-webkit-grabbing" 
          else "move,grab,-moz-grab,-webkit-grab"
        { s with Cursor = cursor }) 

    let { Internal.ChartContext.Chart = chart } = 
      coreChart
      |> initChart (width, height) state.XData state.YData state.ChartOptions 
      |> applyScales 
      |> applyLabels 
      |> applyAxes false false true true

    let chart =
      Padding((20., 0., 0., 0.), chart)
      |> applyInteractive
          ( if state.Completed then []
            else
              [ MouseUp(fun _ _ -> trigger(StopMoving))
                TouchEnd(fun _ -> trigger(StopMoving))
                MouseMove(fun evt (Cont x, Cont y) -> 
                  if (int evt.buttons) &&& 1 = 1 then trigger(ContinueMoving y) )
                TouchMove(fun evt (Cont x, Cont y) -> trigger(ContinueMoving y) )
                MouseDown(fun evt (Cont x, Cont y) -> trigger(StartMoving y) ) 
                TouchStart(fun evt (Cont x, Cont y) -> trigger(StartMoving y) ) ])
    
    h?div ["style"=>"text-align:center;padding-top:20px"] [
      yield h?div ["style" => sprintf "width:%dpx; height:0px; margin:0px auto 0px auto" (int width) ] [
        h?div ["style" => 
          "pointer-events:none; move,grab,-moz-grab,-webkit-grab;" +
          sprintf "opacity:%f; width:50px; height:70px; border-radius:15px; background:#808080;" (0.5 * (1. - state.OverlayOpacity)) + 
          sprintf "position:relative; left:%dpx;top:%dpx" (int (width - 50.) / 2 + 45) (int (height - 70.) / 2 - 30) ] [
          h?i [ "style" => "margin:12px 0px 0px 1px;font-size:42px;color:#f0f0f0"; "class" => "fa fa-hand-o-down" ] []
        ]
      ]

      yield Compost.createSvg false state.IsKeyDate (width, height) chart
      if state.Completed && solvedComment.IsSome then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?p ["class"=>"solved"] [text solvedComment.Value]
        ]
      elif interactive then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?button [
              yield "type" => "button"
              yield "click" =!> fun _ _ -> trigger ShowResults
              if state.OverlayOpacity < 1. then
                yield "disabled" => "disabled"
            ] [ text "Show me how I did" ]
          ]
    ]
            
// ------------------------------------------------------------------------------------------------
// You Guess Bar & You Guess Column
// ------------------------------------------------------------------------------------------------

module YouGuessColsHelpers = 
  open Internal

  type YouGuessState = 
    { Completed : bool
      CompletionStep : float
      Default : float
      Maximum : float
      XData : obj[]
      YData : obj[]
      Data : (string * float)[]
      Guesses : Map<string, float> }

  type YouGuessEvent = 
    | ShowResults 
    | Animate 
    | Update of string * float

  let initState completed isBar data maxValue =     
    { Completed = completed
      CompletionStep = 0.0
      Data = data 
      XData = if isBar then arrayMap (snd >> box) data else arrayMap (fst >> box) data
      YData = if isBar then arrayMap (fst >> box) data else arrayMap (snd >> box) data
      Default = Array.averageBy snd data
      Maximum = InteractiveHelpers.calclateMax maxValue data
      Guesses = if completed then Map.ofSeq data else Map.empty }

  let update log state evt = 
    let collectData () = state.Data |> Array.map (fun (k, v) -> [| box k; box v |]) |> box
    let collectGuesses () = state.Guesses |> Seq.map (fun (KeyValue(k, v)) -> [| box k; box v |]) |> Array.ofSeq |> box
    match evt with
    | ShowResults -> 
        log "completed" [ "guess", collectGuesses(); "values", collectData() ]
        { state with Completed = true }
    | Animate -> { state with CompletionStep = min 1.0 (state.CompletionStep + 0.05) }
    | Update(k, v) -> { state with Guesses = Map.add k v state.Guesses }

  let renderCols (solvedComment:string option) interactive (width, height) topLabel trigger state = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    let chart = 
      Axes(false, false, true, true, 
        AutoScale(false, true, 
          Interactive
            ( ( if state.Completed then []
                else
                  [ EventHandler.MouseMove(fun evt (Cat(x, _), Cont y) ->
                      if (int evt.buttons) &&& 1 = 1 then trigger (Update(x, y)) )
                    EventHandler.MouseDown(fun evt (Cat(x, _), Cont y) ->
                      trigger (Update(x, y)) )
                    EventHandler.TouchStart(fun evt (Cat(x, _), Cont y) ->
                      trigger (Update(x, y)) )
                    EventHandler.TouchMove(fun evt (Cat(x, _), Cont y) ->
                      trigger (Update(x, y)) ) ] ),
              Style
                ( (fun s -> if state.Completed then s else { s with Cursor = "row-resize" }),
                  (Layered [
                    yield Stack
                      ( Horizontal, 
                        [ for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data -> 
                            let sh = Style((fun s -> { s with Fill = Solid(0.2, HTML "#a0a0a0") }), Column(CA lbl, CO state.Maximum )) 
                            Shape.Padding((0., 10., 0., 10.), sh) ])
                    yield Stack
                      ( Horizontal, 
                        [ for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data -> 
                            let alpha, value = 
                              match state.Completed, state.Guesses.TryFind lbl with
                              | true, Some guess -> 0.6, state.CompletionStep * value + (1.0 - state.CompletionStep) * guess
                              | _, Some v -> 0.6, v
                              | _, None -> 0.2, state.Default
                            let sh = Style((fun s -> { s with Fill = Solid(alpha, HTML clr) }), Column(CA lbl, CO value)) 
                            Shape.Padding((0., 10., 0., 10.), sh) ])
                    for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data do
                      match state.Guesses.TryFind lbl with
                      | None -> () 
                      | Some guess ->
                          let line = Line [ CAR(CA lbl, 0.0), COV (CO guess); CAR(CA lbl, 1.0), COV (CO guess) ]
                          yield Style(
                            (fun s -> 
                              { s with
                                  StrokeColor = (1.0, HTML clr)
                                  StrokeWidth = Pixels 4
                                  StrokeDashArray = [ Integer 5; Integer 5 ] }), 
                            Shape.Padding((0., 10., 0., 10.), line))
                    match topLabel with
                    | None -> ()
                    | Some lbl ->
                        let x = CAR(CA (fst state.Data.[state.Data.Length/2]), if state.Data.Length % 2 = 0 then 0.0 else 0.5)
                        let y = COV(CO (state.Maximum * 0.9))
                        yield Style(
                          (fun s -> { s with Font = "13pt Roboto,sans-serif"; Fill=Solid(1.0, HTML "#808080"); StrokeColor=(0.0, RGB(0,0,0)) }),
                          Text(x, y, VerticalAlign.Baseline, HorizontalAlign.Center, 0., lbl) )
                  ]) ))))

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      yield Compost.createSvg false false (width, height) chart
      if state.Completed && solvedComment.IsSome then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?p ["class"=>"solved"] [text solvedComment.Value]
        ]
      elif interactive then 
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?button [
              yield "type" => "button"
              yield "click" =!> fun _ _ -> trigger ShowResults
              if state.Guesses.Count <> state.Data.Length then
                yield "disabled" => "disabled"
            ] [ text "Show me how I did" ]
          ]
    ]


  let renderBars (solvedComment:string option) interactive inlineLabels size chartOptions trigger state = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore

    let chartOptions = 
      if (*isBar && *)inlineLabels && chartOptions.yAxis.labelOffset.IsNone then { chartOptions with yAxis = { chartOptions.yAxis with labelOffset = Some 10. } }
      //elif not isBar && inlineLabels then { chartOptions with xAxis = { chartOptions.xAxis with labelOffset = Some 10. } }
      else chartOptions

    let chart = 
      Layered [
        yield 
          Stack
            ( Vertical, 
              [ for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data -> 
                  let sh = Style((fun s -> { s with Fill = Solid(0.2, HTML "#a0a0a0") }), Bar(CO state.Maximum, CA lbl)) 
                  Shape.Padding((10., 0., 10., 0.), sh) ])
        yield Stack
          ( Vertical, 
            [ for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data -> 
                let alpha, value = 
                  match state.Completed, state.Guesses.TryFind lbl with
                  | true, Some guess -> 0.6, state.CompletionStep * value + (1.0 - state.CompletionStep) * guess
                  | _, Some v -> 0.6, v
                  | _, None -> 0.2, state.Default
                let sh = Style((fun s -> { s with Fill = Solid(alpha, HTML clr) }), Bar(CO value, CA lbl)) 
                Shape.Padding((10., 0., 10., 0.), sh) ])

        if inlineLabels then
          for clr, (lbl, _) in Seq.zip (infinitely vega10) state.Data do 
            let x = COV(CO (state.Maximum * 0.95))
            let y = CAR(CA lbl, 0.5)
            yield Style(
              (fun s -> { s with Font = "13pt Roboto,sans-serif"; Fill=Solid(1.0, HTML clr); StrokeColor=(0.0, RGB(0,0,0)) }),
              Text(x, y, VerticalAlign.Middle, HorizontalAlign.End, 0., lbl) )

        for clr, (lbl, value) in Seq.zip (infinitely vega10) state.Data do
          match state.Guesses.TryFind lbl with
          | None -> () 
          | Some guess ->
              let line = Line [ COV (CO guess), CAR(CA lbl, 0.0); COV (CO guess), CAR(CA lbl, 1.0) ]
              yield Style(
                (fun s -> 
                  { s with
                      StrokeColor = (1.0, HTML clr)
                      StrokeWidth = Pixels 4
                      StrokeDashArray = [ Integer 5; Integer 5 ] }), 
                Shape.Padding((10., 0., 10., 0.), line)) ]
      |> applyInteractive
            ( if state.Completed then []
              else
                [ EventHandler.MouseMove(fun evt (Cont x, Cat(y, _)) ->
                    if (int evt.buttons) &&& 1 = 1 then trigger (Update(y, x)) )
                  EventHandler.MouseDown(fun evt (Cont x, Cat(y, _)) ->
                    trigger (Update(y, x)) )
                  EventHandler.TouchStart(fun evt (Cont x, Cat(y, _)) ->
                    trigger (Update(y, x)) )
                  EventHandler.TouchMove(fun evt (Cont x, Cat(y, _)) ->
                    trigger (Update(y, x)) ) ] )
      |> applyStyle (fun s -> 
          if state.Completed then s else { s with Cursor = "col-resize" })

    let ctx = 
      chart
      |> initChart size state.XData state.YData chartOptions
      |> applyScales 
      |> applyLabels
      //|> applyAxes false false (not (inlineLabels && not isBar)) (not (inlineLabels && isBar))
      |> applyAxes false false true (not inlineLabels)
      |> applyLegend size (Seq.zip (infinitely vega10) (Seq.map fst state.Data))
    
    h?div ["style"=>"text-align:center;padding-top:20px"] [
      yield Compost.createSvg false false size ctx.Chart
      if state.Completed && solvedComment.IsSome then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?p ["class"=>"solved"] [text solvedComment.Value]
        ]
      elif interactive then 
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?button [
              yield "type" => "button"
              yield "click" =!> fun _ _ -> trigger ShowResults
              if state.Guesses.Count <> state.Data.Length then
                yield "disabled" => "disabled"
            ] [ text "Show me how I did" ]
          ]
    ]

// ------------------------------------------------------------------------------------------------
// You Guess Sort Bars
// ------------------------------------------------------------------------------------------------

module YouGuessSortHelpers = 
  type YouGuessState = 
    { Data : (string * float)[] 
      Colors : System.Collections.Generic.IDictionary<string, string>
      Assignments : Map<string, string>
      Selected : string
      Maximum : float 
      CompletionStep : float
      Completed : bool }

  type YouGuessEvent = 
    | SelectItem of string
    | AssignCurrent of string
    | ShowResults 
    | Animate 

  let initState completed maxValue data =     
    { Data = data 
      CompletionStep = 0.0
      Completed = completed
      Colors = Seq.map2 (fun (lbl, _) clr -> lbl, clr) data vega10 |> dict 
      Selected = fst (Seq.head data)
      Maximum = InteractiveHelpers.calclateMax maxValue data 
      Assignments = 
        if completed then Map.ofSeq [for k, _ in data -> k, k]
        else Map.empty }

  let update log state evt = 
    let collectData () = state.Data |> Array.map (fun (k, v) -> [| box k; box v |]) |> box
    let collectGuesses () = 
      state.Assignments |> Seq.map (fun (KeyValue(vk, k)) -> 
        let v = state.Data |> Seq.pick (fun (k, v) -> if k = vk then Some v else None)
        [| box k; box v |]) |> Array.ofSeq |> box
    match evt with
    | Animate -> { state with CompletionStep = min 1.0 (state.CompletionStep + 0.05) }
    | ShowResults -> 
        log "completed" [ "values", collectData (); "guess", collectGuesses () ] 
        { state with Completed = true }
    | SelectItem s -> 
        log "select" [ "selection", box s ]
        { state with Selected = s }
    | AssignCurrent target -> 
        let newAssigns = 
          state.Assignments
          |> Map.filter (fun _ v -> v <> state.Selected)
          |> Map.add target state.Selected
        let assigned = newAssigns |> Seq.map (fun kvp -> kvp.Value) |> set
        let newSelected = 
          state.Data
          |> Seq.map fst 
          |> Seq.filter (assigned.Contains >> not)
          |> Seq.tryHead
        { state with Assignments = newAssigns; Selected = defaultArg newSelected state.Selected }
  
  let renderBars (solvedComment:string option) interactive (width, height) trigger (state:YouGuessState) = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    let chart = 
      Axes(false, false, true, false, 
        AutoScale(true, false,
          Interactive
            ( ( if state.Completed then [] else
                  [ EventHandler.MouseDown(fun evt (_, Cat(y, _)) -> trigger(AssignCurrent y))
                    EventHandler.TouchStart(fun evt (_, Cat(y, _)) -> trigger(AssignCurrent y))
                    EventHandler.TouchMove(fun evt (_, Cat(y, _)) -> trigger(AssignCurrent y)) ]),
              Style
                ( (fun s -> if state.Completed then s else { s with Cursor = "pointer" }),
                  (Layered [
                    yield Stack
                      ( Vertical, 
                        [ for i, (lbl, original) in Seq.mapi (fun i v -> i, v) (Seq.sortBy snd state.Data) do
                            let alpha, value, clr = 
                              match state.Completed, state.Assignments.TryFind lbl with
                              | true, Some assigned -> 
                                  let _, actual = state.Data |> Seq.find (fun (lbl, _) -> lbl = assigned)
                                  0.6, state.CompletionStep * actual + (1.0 - state.CompletionStep) * original, state.Colors.[assigned]
                              | _, Some assigned -> 0.6, original, state.Colors.[assigned]
                              | _, None -> 0.3, original, "#a0a0a0"

                            if i = state.Data.Length - 1 && state.Assignments.Count = 0 then
                              let txt = Text(COV(CO(state.Maximum * 0.05)), CAR(CA lbl, 0.5), Middle, Start, 0., "Assign highlighted value to one of the bars by clicking on it!")
                              yield Style((fun s -> { s with Font = "13pt Roboto,sans-serif"; Fill = Solid(1.0, HTML "#606060"); StrokeColor=(0.0, HTML "white") }), txt ) 

                            let sh = Style((fun s -> { s with Fill = Solid(alpha, HTML clr) }), Bar(CO value, CA lbl)) 
                            if clr <> "#a0a0a0" then
                              let line = Line [ COV (CO original), CAR(CA lbl, 0.0); COV (CO original), CAR(CA lbl, 1.0) ]
                              yield Style(
                                (fun s -> 
                                  { s with
                                      StrokeColor = (1.0, HTML clr)
                                      StrokeWidth = Pixels 4
                                      StrokeDashArray = [ Integer 5; Integer 5 ] }), 
                                Shape.Padding((5., 0., 5., 0.), line))
                            yield Shape.Padding((5., 0., 5., 0.), sh) ])
                  ]) ))))

    let labs = 
      Padding(
        (0., 20., 20., 25.),
        InnerScale(Some(CO 0., CO 100.), None, 
          Interactive(
            ( if state.Completed then [] else
                [ EventHandler.MouseDown(fun evt (_, Cat(lbl, _)) -> trigger (SelectItem lbl))
                  EventHandler.TouchStart(fun evt (_, Cat(lbl, _)) -> trigger (SelectItem lbl)) 
                  EventHandler.TouchMove(fun evt (_, Cat(lbl, _)) -> trigger (SelectItem lbl)) ]),
            Layered
              [ for lbl, _ in Seq.rev state.Data do
                  let clr = state.Colors.[lbl]
                  let x = COV(CO 5.)
                  let y = CAR(CA lbl, 0.5)
                  let af, al = if state.Completed || lbl = state.Selected then 0.9, 1.0 else 0.2, 0.6
                  yield 
                    Style((fun s -> { s with Fill=Solid(af, HTML clr)  }), 
                      Padding((2., 0., 2., 0.), Bar(CO 4., CA lbl)))
                  yield Style(
                    (fun s -> { s with Font = "11pt Roboto,sans-serif"; Fill=Solid(al, HTML clr); StrokeColor=(0.0, RGB(0,0,0)) }),
                    Text(x, y, VerticalAlign.Middle, HorizontalAlign.Start, 0., lbl) ) 
                  if not state.Completed then
                    yield 
                      Style((fun s -> { s with Cursor="pointer"; Fill=Solid(0.0, HTML "white")  }), Bar(CO 100., CA lbl))
                ])))
             
    let all = 
      Layered
        [ OuterScale(None, Some(Continuous(CO 0.0, CO 3.0)), labs) 
          OuterScale(None, Some(Continuous(CO 3.0, CO 10.0)), Padding((0.,20.,0.,20.), chart)) ]

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      yield Compost.createSvg false false (width, height) all
      if state.Completed && solvedComment.IsSome then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?p ["class"=>"solved"] [text solvedComment.Value]
        ]
      elif interactive then
        yield h?div ["style"=>"padding-bottom:20px"] [
          h?button [
              yield "type" => "button"
              yield "click" =!> fun _ _ -> trigger ShowResults
              if state.Assignments.Count <> state.Data.Length then
                yield "disabled" => "disabled"
            ] [ text "Show me how I did" ]
          ]
    ]
    
// ------------------------------------------------------------------------------------------------
// You Guess API
// ------------------------------------------------------------------------------------------------

open TheGamma.Series
open TheGamma.Common

type YouGuessColsBarsKind = Cols | Bars

type YouGuessColsBars =
  private 
    { kind : YouGuessColsBarsKind
      data : series<string, float> 
      logger : (obj -> unit) option
      interactive : bool
      solvedComment : string option
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setSolvedComment(comment) = { y with solvedComment = Some comment }
  member y.setLogger(logger) = { y with logger = Some logger }
  member y.setLabel(top:string) = y.setTitle(top) // TODO: Deprecated
  member y.setMaximum(max:float) = if y.kind = Bars then y.setAxisX(maxValue=box max) else y.setAxisY(maxValue=box max) // TODO: Deprecated
  member y.setInteractive(state) = { y with interactive = state }
  member y.show(outputId) =   
    InteractiveHelpers.showApp outputId y.options y.data
      (fun data -> YouGuessColsHelpers.initState (not y.interactive) (y.kind = Bars) data (unbox (if y.kind = Bars then y.options.xAxis.maxValue else y.options.yAxis.maxValue)))
      (fun _ size -> 
          match y.kind with 
          | Bars -> YouGuessColsHelpers.renderBars y.solvedComment y.interactive true size y.options
          | Cols -> YouGuessColsHelpers.renderCols y.solvedComment y.interactive size y.options.title)
      (fun _ -> YouGuessColsHelpers.update (InteractiveHelpers.createLogger outputId y.logger) )

type YouGuessLine = 
  private 
    { data : series<obj, float> 
      markers : series<obj, obj> option
      clip : float option
      markerColor : string option
      knownColor : string option
      unknownColor : string option 
      drawColor : string option 
      knownLabel : string option
      guessLabel : string option 
      logger : (obj -> unit) option
      interactive : bool
      solvedComment : string option
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setSolvedComment(comment) = { y with solvedComment = Some comment }
  member y.setLogger(logger) = { y with logger = Some logger }
  member y.setRange(min, max) = y.setAxisY(min, max) // TODO: Deprecated
  member y.setClip(clip) = { y with clip = Some (dateOrNumberAsNumber clip) }
  member y.setColors(known, unknown) = { y with knownColor = Some known; unknownColor = Some unknown }
  member y.setDrawColor(draw) = { y with drawColor = Some draw }
  member y.setMarkerColor(marker) = { y with markerColor = Some marker }
  member y.setLabels(?top, ?known, ?guess) = 
    { y with knownLabel = orElse known y.knownLabel; guessLabel = orElse guess y.guessLabel; options = { y.options with title = orElse top y.options.title } }
  member y.setMarkers(markers) = { y with markers = Some markers }
  member y.setInteractive(state) = { y with interactive = state }
  member y.show(outputId) = Async.StartImmediate <| async {
    let markers = defaultArg y.markers (series<string, float>.create(async.Return [||], "", "", ""))
    let! markers = markers.data |> Async.AwaitFuture
    let markers = markers |> Array.map (fun (k, v) -> dateOrNumberAsNumber k, v) |> Array.sortBy fst
    return! InteractiveHelpers.showAppAsync outputId y.options y.data
      (fun data ->
          let clipx = match y.clip with Some v -> v | _ -> dateOrNumberAsNumber (fst (data.[data.Length / 2]))
          YouGuessLineHelpers.initState (not y.interactive) (Array.sortBy (fst >> dateOrNumberAsNumber) data) clipx)
      (fun data size trig ->           
          let lc, dc, gc, mc = 
            defaultArg y.knownColor "#606060", defaultArg y.unknownColor "#FFC700", 
            defaultArg y.drawColor "#808080", defaultArg y.markerColor "#C65E31"    
          let data = Array.sortBy (fst >> dateOrNumberAsNumber) data
          let co = { y.options with xAxis = { y.options.xAxis with minValue = Some (box (dateOrNumberAsNumber (fst data.[0]))); maxValue = Some (box (dateOrNumberAsNumber (fst data.[data.Length-1]))) } }
          YouGuessLineHelpers.render y.solvedComment y.interactive co size markers (defaultArg y.knownLabel "", defaultArg y.guessLabel "") (lc,dc,gc,mc) trig) 
      (fun _ -> YouGuessLineHelpers.handler (InteractiveHelpers.createLogger outputId y.logger)) } 

type YouGuessLineOffset = 
  private 
    { data : series<obj, float> 
      markers : series<obj, obj> option
      labels : string[]
      logger : (obj -> unit) option
      interactive : bool
      lineColor : string option 
      guessColor : string option
      markerColor : string option
      solvedComment : string option
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setLabels(labels) = { y with labels = labels }
  member y.setSolvedComment(comment) = { y with solvedComment = Some comment }
  member y.setLogger(logger) = { y with logger = Some logger }
  member y.setColors(?draw, ?solved, ?marker) = { y with guessColor = orElse draw y.guessColor; lineColor = orElse solved y.lineColor; markerColor = orElse marker y.markerColor }
  member y.setMarkers(markers) = { y with markers = Some markers }
  member y.setInteractive(state) = { y with interactive = state }
  member y.show(outputId) = Async.StartImmediate <| async {
    let markers = defaultArg y.markers (series<string, float>.create(async.Return [||], "", "", ""))
    let! markers = markers.data |> Async.AwaitFuture
    let markers = markers |> Array.map (fun (k, v) -> dateOrNumberAsNumber k, v) |> Array.sortBy fst
    return! InteractiveHelpers.showAppAsync outputId y.options y.data
      (fun data ->
          let data = Array.sortBy (fst >> dateOrNumberAsNumber) data
          let lo = defaultArg y.options.yAxis.minValue (box (dateOrNumberAsNumber (fst data.[0])))
          let hi = defaultArg y.options.yAxis.maxValue (box (dateOrNumberAsNumber (fst data.[data.Length-1])))
          let co = { y.options with yAxis = { y.options.yAxis with minValue = Some lo; maxValue = Some hi } }
          YouGeussLineOffsetHelpers.initState (not y.interactive) co (Array.sortBy (fst >> dateOrNumberAsNumber) data))
      (fun data size trig ->           
          let lc = defaultArg y.lineColor "#bcbd22"
          let gc = defaultArg y.guessColor "#808080"
          let mc = defaultArg y.markerColor "#800080"
          YouGeussLineOffsetHelpers.render y.solvedComment y.interactive size y.labels markers (gc, lc, mc) trig) 
      (fun _ -> YouGeussLineOffsetHelpers.handler (InteractiveHelpers.createLogger outputId y.logger)) } 

type YouGuessSortBars = 
  private 
    { data : series<string, float> 
      maxValue : float option 
      size : float option * float option 
      logger : (obj -> unit) option 
      interactive : bool 
      solvedComment : string option }
  member y.setSolvedComment(comment) = { y with solvedComment = Some comment }
  member y.setLogger(logger) = { y with logger = Some logger }
  member y.setMaximum(max) = { y with maxValue = Some max }
  member y.setSize(?width, ?height) = 
    { y with size = (orElse width (fst y.size), orElse height (snd y.size)) }
  member y.setInteractive(state) = { y with interactive = state }
  member y.show(outputId) = 
    InteractiveHelpers.showApp outputId { ChartOptions.Default with size = y.size } y.data
      (YouGuessSortHelpers.initState (not y.interactive) y.maxValue)
      (fun _ size -> YouGuessSortHelpers.renderBars y.solvedComment y.interactive size)
      (fun _ -> YouGuessSortHelpers.update (InteractiveHelpers.createLogger outputId y.logger))

type youguess = 
  static member columns(data:series<string, float>) = 
    { YouGuessColsBars.data = data; kind = Cols; options = ChartOptions.Default; logger = None; interactive = true; solvedComment = None }
  static member bars(data:series<string, float>) = 
    { YouGuessColsBars.data = data; kind = Bars; options = ChartOptions.Default; logger = None; interactive = true; solvedComment = None }
  static member sortBars(data:series<string, float>) = 
    { YouGuessSortBars.data = data; maxValue = None; size = None, None; logger = None; interactive = true; solvedComment = None }
  static member line(data:series<obj, float>) =
    { YouGuessLine.data = data; clip = None; 
      markerColor = None; guessLabel = None; knownLabel = None; markers = None
      knownColor = None; unknownColor = None; drawColor = None; 
      options = ChartOptions.Default; logger = None; interactive = true; solvedComment = None }
  static member vlineOffset(data:series<obj, float>) =
    { YouGuessLineOffset.data = data; markers = None; lineColor = None; 
      guessColor = None; markerColor = None; labels = [| for c in 'A' .. 'Z' -> string c |]
      options = ChartOptions.Default; logger = None; interactive = true; solvedComment = None }

// ------------------------------------------------------------------------------------------------
// Compost Charts API
// ------------------------------------------------------------------------------------------------

type CompostBubblesChartSet =
  private 
    { data : series<obj, obj> 
      selectY : obj -> obj
      selectX : obj -> obj
      selectLabel : option<obj -> obj>
      selectSize : option<obj -> obj>
      bubbleColor : string option
  // [copy-paste]
      options : ChartOptions }
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setColors(?bubbleColor) = 
    { y with bubbleColor = defaultArg bubbleColor y.bubbleColor }
  member y.show(outputId) = 
    InteractiveHelpers.showApp outputId y.options y.data
      (fun data -> -1)
      (fun data size trigger selectedBubble -> 
        let ss f = match f with Some f -> (fun x -> Some(f x)) | _ -> (fun _ -> None)
        let data = data |> Array.map (fun (_, v) -> y.selectX v, y.selectY v, ss y.selectSize v, ss y.selectLabel v)
        let bc = defaultArg y.bubbleColor "#20a030"
        Charts.renderBubbles trigger selectedBubble y.options size bc data)
      (fun _ _ evt -> evt)

type CompostBubblesChart<'k, 'v>(data:series<'k, 'v>) = 
  member c.set(x:'v -> obj, y:'v -> obj, ?size:'v -> obj, ?label:'v -> obj) = 
    { CompostBubblesChartSet.data = unbox data
      selectX = unbox x; selectY = unbox y
      selectSize = unbox size; bubbleColor = None 
      options = ChartOptions.Default; selectLabel = unbox label }


type CompostColBarChart =
  private 
    { isBar : bool
      data : series<string, float>
      colors : string[] option
      inlineLabels : bool
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setStyle(?inlineLabels) = 
    { y with inlineLabels = defaultArg inlineLabels y.inlineLabels }
  member y.setColors(?colors) = 
    { y with colors = defaultArg colors y.colors }
  member y.show(outputId) = 
    InteractiveHelpers.showStaticApp outputId y.options y.data
      (fun data size -> 
        let cc = defaultArg y.colors vega10
        Charts.renderColsBars y.isBar y.inlineLabels y.options size cc (Seq.map fst data) data)


type CompostLineAreaChart =
  private 
    { isArea : bool
      data : series<obj, obj>
      lineColor : string option
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setColors(?lineColor) = 
    { y with lineColor = defaultArg lineColor y.lineColor }
  member y.show(outputId) = 
    InteractiveHelpers.showStaticApp outputId y.options y.data
      (fun data size -> 
        let lc = defaultArg y.lineColor "#1f77b4"
        Charts.renderLines y.isArea y.options size [| lc |] ["Data"] [| data |])


type CompostLinesAreasChart =
  private 
    { isArea : bool
      data : series<obj, obj>[]
      lineColors : string[] option
  // [copy-paste]
      options : ChartOptions }  
  member y.setEmptyMessage(message) =
    { y with options = { y.options with emptyMessage = Some message } }
  member y.setTitle(?title) =
    { y with options = { y.options with title = title } }
  member y.setLegend(position) = 
    { y with options = { y.options with legend = { position = position } } }
  member y.setSize(?width, ?height) = 
    { y with options = { y.options with size = (orElse width (fst y.options.size), orElse height (snd y.options.size)) } }
  member y.setAxisX(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.xAxis with minValue = orElse minValue y.options.xAxis.minValue; maxValue = orElse maxValue y.options.xAxis.maxValue; label = orElse label y.options.xAxis.label; labelOffset = orElse labelOffset y.options.xAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.xAxis.labelMinimalSize }
    { y with options = { y.options with xAxis = ax } }
  member y.setAxisY(?minValue, ?maxValue, ?label, ?labelOffset, ?labelMinimalSize) = 
    let ax = { y.options.yAxis with minValue = orElse minValue y.options.yAxis.minValue; maxValue = orElse maxValue y.options.yAxis.maxValue; label = orElse label y.options.yAxis.label; labelOffset = orElse labelOffset y.options.yAxis.labelOffset; labelMinimalSize = orElse labelMinimalSize y.options.yAxis.labelMinimalSize  }
    { y with options = { y.options with yAxis = ax } }
  // [/copy-paste]
  member y.setColors(?lineColors) = 
    { y with lineColors = defaultArg lineColors y.lineColors }
  member y.show(outputId) = Async.StartImmediate <| async {
    let! data = Async.Parallel [ for d in y.data -> Async.AwaitFuture d.data ]
    let allData = y.data |> Seq.fold (fun (s1:series<_,_>) s2 -> s1.append(s2)) (series<int,int>.create(async.Return [||], "", "", ""))
    return! InteractiveHelpers.showStaticAppAsync outputId y.options allData
      (fun size -> 
        let lcs = defaultArg y.lineColors vega10
        Charts.renderLines y.isArea y.options size lcs (y.data |> Seq.map (fun s -> s.seriesName)) data) }
      
type CompostCharts() = 
  member c.bubbles(data:series<'k, 'v>) = 
    CompostBubblesChart<'k, 'v>(data)
  member c.line(data:series<'k, 'v>) = 
    { CompostLineAreaChart.data = unbox data; lineColor = None; options = ChartOptions.Default; isArea = false }
  member c.area(data:series<'k, 'v>) = 
    { CompostLineAreaChart.data = unbox data; lineColor = None; options = ChartOptions.Default; isArea = true }
  member c.lines(data:series<'k, 'v>[]) = 
    { CompostLinesAreasChart.data = unbox data; lineColors = None; options = ChartOptions.Default; isArea = false }
  member c.areas(data:series<'k, 'v>[]) = 
    { CompostLinesAreasChart.data = unbox data; lineColors = None; options = ChartOptions.Default; isArea = true }
  member c.bar(data:series<string, float>) = 
    { CompostColBarChart.data = data; colors = None; options = ChartOptions.Default; isBar = true; inlineLabels = false }
  member c.column(data:series<string, float>) = 
    { CompostColBarChart.data = data; colors = None; options = ChartOptions.Default; isBar = false; inlineLabels = false }

type compost = 
  static member charts = CompostCharts()
