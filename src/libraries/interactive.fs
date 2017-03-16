namespace TheGamma.Interactive 

open TheGamma.Html
open Fable.Import.Browser
open Fable.Helpers

module Visualizations = 

  // ----------------------------------------------------------------------------------------------
  // Domain that users see
  // ----------------------------------------------------------------------------------------------
  
  type Color =
    | RGB of int * int * int
    | HTML of string

  type AlphaColor = float * Color 
  type Width = Pixels of int
  type GradientStop = float * AlphaColor

  type FillStyle =
    | Solid of AlphaColor
    | LinearGradient of seq<GradientStop>

  type Number =
    | Integer of int
    | Percentage of float

  type Style = 
    { StrokeColor : AlphaColor
      StrokeWidth : Width
      StrokeDashArray : seq<Number>
      Fill : FillStyle 
      Animation : option<int * string * (Style -> Style)>
      Font : string }

  type HorizontalAlign = Start | Center | End
  type VerticalAlign = Baseline | Middle | Hanging

  type continuous<[<Measure>] 'u> = CO of float<'u> 

  type EventHandler<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | MouseMove of (MouseEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | MouseUp of (MouseEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | MouseDown of (MouseEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | TouchStart of (TouchEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | TouchEnd of (TouchEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | TouchMove of (TouchEvent -> (continuous<'vx> * continuous<'vy>) -> unit)
    | MouseLeave of (MouseEvent -> unit)

  type Shape<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | Style of (Style -> Style) * Shape<'vx, 'vy>
    | Text of continuous<'vx> * continuous<'vy> * VerticalAlign * HorizontalAlign * string
    | Scale of option<continuous<'vx> * continuous<'vx>> * option<continuous<'vy> * continuous<'vy>> * Shape<'vx, 'vy>
    | Line of seq<continuous<'vx> * continuous<'vy>>
    | Area of seq<continuous<'vx> * continuous<'vy>>
    | Layered of seq<Shape<'vx, 'vy>>
    | Axes of Shape<'vx, 'vy>
    | Interactive of seq<EventHandler<'vx, 'vy>> * Shape<'vx, 'vy>

  // ----------------------------------------------------------------------------------------------
  // SVG stuff
  // ----------------------------------------------------------------------------------------------
  
  type StringBuilder() = 
    let mutable strs = [] 
    member x.Append(s) = strs <- s::strs
    override x.ToString() = String.concat "" (List.rev strs)

  type PathSegment = 
    | MoveTo of (float * float)
    | LineTo of (float * float)

  type SvgStyle = string

  type Svg =
    | Path of PathSegment[] * SvgStyle
    | Text of (float * float) * string * SvgStyle
    | Combine of Svg[]
    | Empty

  let formatPath path = 
    let sb = StringBuilder()
    for ps in path do
      match ps with
      | MoveTo(x, y) -> sb.Append("M" + string x + " " + string y + " ")
      | LineTo(x, y) -> sb.Append("L" + string x + " " + string y + " ")
    sb.ToString()

  let rec renderSvg svg = seq { 
    match svg with
    | Empty -> ()
    | Text((x,y), t, style) ->
        yield s?text [ "x" => string x; "y" => string y; "style" => style ] [text t]
    | Combine ss ->
        for s in ss do yield! renderSvg s
    | Path(p, style) ->
        yield s?path [ "d" => formatPath p; "style" => style ] [] }

  let formatColor = function
    | RGB(r,g,b) -> sprintf "rgb(%d, %d, %d)" r g b
    | HTML(clr) -> clr

  let formatNumber = function
    | Integer n -> string n
    | Percentage p -> string p + "%"

  let rec formatStyle (defs:ResizeArray<_>) style = 
    let style, anim =
      match style.Animation with 
      | Some (ms, ease, anim) ->
          let id = "anim_" + System.Guid.NewGuid().ToString().Replace("-", "")
          let fromstyle = formatStyle defs { style with Animation = None }
          let tostyle = formatStyle defs { anim style with Animation = None }
          h?style [] [ text (sprintf "@keyframes %s { from { %s } to { %s } }" id fromstyle tostyle) ] |> defs.Add
          anim style, sprintf "animation: %s %dms %s; " id ms ease
      | None -> style, ""

    anim +
    ( "font:" + style.Font + ";" ) +
    ( let (so, clr) = style.StrokeColor 
      let (Pixels sw) = style.StrokeWidth
      sprintf "stroke-opacity:%f; stroke-width:%dpx; stroke:%s; " so sw (formatColor clr) ) +
    ( if Seq.isEmpty style.StrokeDashArray then "" 
      else "stroke-dasharray:" + String.concat "," (Seq.map formatNumber style.StrokeDashArray) + ";" ) +
    ( match style.Fill with
      | LinearGradient(points) ->
          let id = "gradient_" + System.Guid.NewGuid().ToString().Replace("-", "")
          s?linearGradient ["id"=>id] 
            [ for pt, (o, clr) in points ->
                s?stop ["offset"=> string pt + "%"; "stop-color" => formatColor clr; "stop-opacity" => string o ] [] ]
          |> defs.Add
          sprintf "fill:url(#%s)" id
      | Solid(fo, clr) ->
          sprintf "fill-opacity:%f; fill:%s; " fo (formatColor clr) )

  // ----------------------------------------------------------------------------------------------
  // Calculating scales
  // ----------------------------------------------------------------------------------------------

  type Scale<[<Measure>] 'v> =
    | Continuous of continuous<'v> * continuous<'v>

  type ScaledShapeInner<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | ScaledStyle of (Style -> Style) * ScaledShape<'vx, 'vy>
    | ScaledText of continuous<'vx> * continuous<'vy> * VerticalAlign * HorizontalAlign * string    
    | ScaledLine of (continuous<'vx> * continuous<'vy>)[]
    | ScaledArea of (continuous<'vx> * continuous<'vy>)[]
    | ScaledLayered of ScaledShape<'vx, 'vy>[]
    | ScaledAxes of   
        limits : ((continuous<'vx> * continuous<'vx>) * (continuous<'vy> * continuous<'vy>)) *
        grid : (continuous<'vx>[] * continuous<'vy>[]) * 
        labels : ((continuous<'vx>*string)[] * (continuous<'vy>*string)[]) * 
        shape : ScaledShape<'vx, 'vy>
    | ScaledInteractive of seq<EventHandler<'vx, 'vy>> * ScaledShape<'vx, 'vy>

  and ScaledShape<[<Measure>] 'vx, [<Measure>] 'vy> =
    Scaled of (Scale<'vx> * Scale<'vy>) * ScaledShapeInner<'vx, 'vy>


  [<Fable.Core.Emit("$0.toFixed($1)")>]
  let toFixed (num:float) (decs:float) : string = failwith "JS"

  let generateRange (CO (lo:float<'u>)) (CO (hi:float<'u>)) = 
    let lo, hi = unbox lo, unbox hi 
    let mag = 10. ** round (log10 (hi - lo))
    let decimals = max 0. (-(log10 mag))
    let alo, ahi = floor (lo / mag) * mag, ceil (hi / mag) * mag
    let range = (ahi - alo) / mag
    let mag, range = if range >= 10. then mag, range else mag/10.0, range * 10.0 // maybe floor log when calculating mag instead?
    let tmag = mag * (floor (range / 5.)) // generate ~5 text labels
    let gmag = mag * (floor (range / 10.)) // generate ~10 grid lines
    (CO (unbox<float<'u>> alo), CO (unbox<float<'u>> ahi)),
    [| for v in alo .. gmag .. ahi -> CO (unbox<float<'u>> v) |],
    [| for v in alo .. tmag .. ahi -> CO (unbox<float<'u>> v), toFixed v decimals |]

  let unionScales s1 s2 =
    match s1, s2 with
    | Continuous(l1, h1), Continuous(l2, h2) -> Continuous(min l1 l2, max h1 h2)

  // Replace scales in all immediately nested things that will
  // share the same scale when combined via Layered
  // (recursively over Interacitve & Layered with Line as leaf)

  let rec replaceScales scales (Scaled(_, shape) as scaled) =
    match shape with
    | ScaledLine _ 
    | ScaledText _
    | ScaledArea _ -> Scaled(scales, shape)
    | ScaledStyle(f, shape) -> Scaled(scales, ScaledStyle(f, replaceScales scales shape))
    | ScaledInteractive(f, shape) -> Scaled(scales, ScaledInteractive(f, replaceScales scales shape))
    | ScaledLayered(shapes) -> Scaled(scales, ScaledLayered(Array.map (replaceScales scales) shapes))
    | ScaledAxes _ -> scaled

  // From the leafs to the root, calculate the scales of
  // everything (composing sales of leafs to get scale of root)

  let calculateLineOrAreaScales line = 
    let xs = line |> Array.map fst 
    let ys = line |> Array.map snd
    let x0, x1 = Array.min xs, Array.max xs
    let y0, y1 = Array.min ys, Array.max ys
    Continuous(x0, x1), Continuous(y0, y1)

  let rec calculateScales<[<Measure>] 'ux, [<Measure>] 'uy> (shape:Shape<'ux, 'uy>) = 
    match shape with
    | Scale(sx, sy, shape) ->
        let (Scaled((asx, asy), shape)) = calculateScales shape
        let scales = 
          (match sx with Some sx -> Continuous(sx) | _ -> asx), 
          (match sy with Some sy -> Continuous(sy) | _ -> asy) 
        Scaled(scales, shape) |> replaceScales scales

    | Style(style, shape) ->
        let (Scaled(scales, shape)) = calculateScales shape
        Scaled(scales, ScaledStyle(style, Scaled(scales, shape)))

    | Shape.Text(x, y, va, ha, t) ->
        Scaled((Continuous(x, x), Continuous(y, y)), ScaledText(x, y, va, ha, t))

    | Line line -> 
        let line = Seq.toArray line 
        let sx, sy = calculateLineOrAreaScales line
        Scaled((sx, sy), ScaledLine(line))

    | Area area -> 
        let area = Seq.toArray area
        let sx, sy = calculateLineOrAreaScales area
        Scaled((sx, sy), ScaledArea(area))

    | Axes shape ->
        let scaled = calculateScales shape        
        let (Scaled((Continuous(lx, hx), Continuous(ly, hy)), _)) = scaled
        let (limx, gx , lblx), (limy, gy, lbly) = generateRange lx hx, generateRange ly hy
        let scales = Continuous(limx), Continuous(limy)
        Scaled(scales, ScaledAxes((limx, limy), (gx, gy), (lblx, lbly), scaled))

    | Layered shapes ->
        let shapes = shapes |> Array.ofSeq
        let scaled = shapes |> Array.map calculateScales 
        let sxs = scaled |> Array.map (fun (Scaled((sx, _), _)) -> sx)
        let sys = scaled |> Array.map (fun (Scaled((_, sy), _)) -> sy)
        let scales = (Array.reduce unionScales sxs, Array.reduce unionScales sys)
        Scaled(scales, ScaledLayered scaled) |> replaceScales scales 

    | Interactive(f, shape) ->
        let (Scaled(scale, shape)) = calculateScales shape
        Scaled(scale, ScaledInteractive(f, Scaled(scale, shape)))

  // ----------------------------------------------------------------------------------------------
  // Calculate projections
  // ----------------------------------------------------------------------------------------------

  type Projection<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> = 
    | Scale of (float<'ux> * float<'ux>) * (float<'uy> * float<'uy>)
    | Padding of float<'uy> * float<'ux> * float<'uy> * float<'ux> * Projection<'vx, 'vy, 'ux, 'uy>

  type ProjectedShapeInner<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | ProjectedStyle of (Style -> Style) * ProjectedShape<'vx, 'vy>
    | ProjectedText of continuous<'vx> * continuous<'vy> * VerticalAlign * HorizontalAlign * string    
    | ProjectedLine of (continuous<'vx> * continuous<'vy>)[]
    | ProjectedArea of (continuous<'vx> * continuous<'vy>)[]
    | ProjectedLayered of ProjectedShape<'vx, 'vy>[]
    | ProjectedAxes of   
        limits : ((continuous<'vx> * continuous<'vx>) * (continuous<'vy> * continuous<'vy>)) *
        grid : (continuous<'vx>[] * continuous<'vy>[]) * 
        labels : ((continuous<'vx>*string)[] * (continuous<'vy>*string)[]) * 
        shape : ProjectedShape<'vx, 'vy>
    | ProjectedInteractive of seq<EventHandler<'vx, 'vy>> * ProjectedShape<'vx, 'vy>

  and ProjectedShape<[<Measure>] 'vx, [<Measure>] 'vy> =
    Projected of Projection<'vx, 'vy, 1, 1> * (Scale<'vx> * Scale<'vy>) * ProjectedShapeInner<'vx, 'vy>


  let scaleOne (tlv:float<_>, thv:float<_>) scale coord = 
    match scale, coord with
    | Continuous(CO slv, CO shv), (CO v) ->
        CO((v - slv) / (shv - slv) * (thv - tlv) + tlv)

  let rec project<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> 
      (sx:Scale<'vx>) (sy:Scale<'vy>) point (projection:Projection<'vx, 'vy, 'ux, 'uy>) : continuous<'ux> * continuous<'uy> = 
    match projection, point with
    | Padding(t,r,b,l,projection), (CO x, CO y) ->
        let (Continuous(lx, hx), Continuous(ly, hy)) = sx, sy
        let (CO x1, CO y1) = project sx sy (lx, ly) projection
        let (CO x2, CO y2) = project sx sy (hx, hy) projection
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2

        let (CO x, CO y) = project sx sy point projection

        // Assuming the result is in pixels...
        let nx = lx + l + (hx - lx - l - r) / (hx - lx) * (x - lx)
        let ny = ly + t + (hy - ly - t - b) / (hy - ly) * (y - ly)
        (CO nx, CO ny)

    | Scale(tx, ty), (x, y) ->
        scaleOne tx sx x, scaleOne ty sy y 

  let rec calculateProjections<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ScaledShape<'ux, 'uy>) projection = 
    match shape with
    | Scaled(scales, ScaledStyle(style, shape)) ->
        Projected(projection, scales, ProjectedStyle(style, calculateProjections shape projection))

    | Scaled(scales, ScaledLine line) -> 
        Projected(projection, scales, ProjectedLine line)

    | Scaled(scales, ScaledText(x, y, va, ha, t)) -> 
        Projected(projection, scales, ProjectedText(x, y, va, ha, t))

    | Scaled(scales, ScaledArea area) -> 
        Projected(projection, scales, ProjectedArea area)

    | Scaled(scales, ScaledAxes(lim, grid, lbls, shape)) ->
        let ppad = Padding(20.0, 20.0, 40.0, 100.0, projection)
        Projected(ppad, scales, ProjectedAxes(lim, grid, lbls, calculateProjections shape ppad))
        
    | Scaled(scales, ScaledLayered shapes) ->
        Projected(projection, scales, ProjectedLayered(shapes |> Array.map (fun s -> calculateProjections s projection)))

    | Scaled(scales, ScaledInteractive(f, shape)) ->
        Projected(projection, scales, ProjectedInteractive(f, calculateProjections shape projection))

  // ----------------------------------------------------------------------------------------------
  // Drawing
  // ----------------------------------------------------------------------------------------------

  let rec hideFill style = 
    { style with Fill = Solid(0.0, RGB(0, 0, 0)); Animation = match style.Animation with Some(n,e,f) -> Some(n,e,f >> hideFill) | _ -> None }
  let rec hideStroke style = 
    { style with StrokeColor = (0.0, snd style.StrokeColor); Animation = match style.Animation with Some(n,e,f) -> Some(n,e,f >> hideStroke) | _ -> None }

  let rec drawShape<[<Measure>] 'ux, [<Measure>] 'uy> defs style (shape:ProjectedShape<'ux, 'uy>) = 
    let (Projected(projection, (sx, sy), shape)) = shape
    let (Continuous(lx, hx), Continuous(ly, hy)) = sx, sy

    let projectCont (x, y) = 
      match project sx sy (x, y) projection with
      | (CO x), (CO y) -> x, y

    match shape with
    | ProjectedStyle(sf, shape) ->
        drawShape defs (sf style) shape

    | ProjectedText(x, y, va, ha, t) -> 
        let va = match va with Baseline -> "baseline" | Hanging -> "hanging" | Middle -> "middle"
        let ha = match ha with Start -> "start" | Center -> "middle" | End -> "end"
        Text(projectCont (x, y), t, sprintf "alignment-baseline:%s; text-anchor:%s;" va ha + formatStyle defs style)

    | ProjectedLine line -> 
        let path = 
          [ yield MoveTo(projectCont (Seq.head line)) 
            for pt in Seq.skip 1 line do yield LineTo (projectCont pt) ]
          |> Array.ofList
        Path(path, formatStyle defs (hideFill style)) 

    | ProjectedArea line -> 
        let firstX, lastX = fst (Seq.head line), fst (Seq.last line)
        let path = 
          [ yield MoveTo(projectCont (firstX, ly))
            for pt in line do yield LineTo (projectCont pt) 
            yield LineTo(projectCont (lastX, ly))
            yield LineTo(projectCont (firstX, ly)) ]
          |> Array.ofList        
        Path(path, formatStyle defs (hideStroke style)) 

    | ProjectedAxes(((lox, hix), (loy, hiy)), (gridx, gridy), (lblx, lbly), shape) ->
        let offs (dx, dy) (x, y) = (x+dx, y+dy)
        Combine   
           [| yield Path(
                [|yield MoveTo (projectCont (lx, hy)) 
                  yield LineTo (projectCont (lx, ly))
                  yield LineTo (projectCont (hx, ly)) |], "fill:transparent; stroke:rgb(0,0,0); stroke-width:2");
              for x, xl in lblx do 
                yield Text(offs (0., 10.0) (projectCont (x, ly)), xl, "alignment-baseline:hanging;text-anchor:middle;font:9pt sans-serif")
              for y, yl in lbly do 
                yield Text(offs (-10., 0.0) (projectCont (lx, y)), yl, "alignment-baseline:middle;text-anchor:end;font:9pt sans-serif")
              yield Path(
                [|for x in gridx do
                    yield MoveTo (projectCont (x, ly))
                    yield LineTo (projectCont (x, hy))
                  for y in gridy do
                    yield MoveTo (projectCont (lx, y))
                    yield LineTo (projectCont (hx, y)) |], "fill:transparent; stroke:rgb(228,228,228); stroke-width:1") 
              yield drawShape defs style shape |]     

    | ProjectedLayered shapes ->
        Combine(shapes |> Array.map (fun s -> drawShape defs style s))

    | ProjectedInteractive(f, shape) ->
        drawShape defs style shape

  // ----------------------------------------------------------------------------------------------
  // Event handling
  // ----------------------------------------------------------------------------------------------

  type MouseEventKind = Move | Up | Down
  type TouchEventKind = Move | End | Start 

  type InteractiveEvent<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | MouseEvent of MouseEventKind * (continuous<'vx> * continuous<'vy>)    
    | TouchEvent of TouchEventKind * (continuous<'vx> * continuous<'vy>)    
    | MouseLeave

  // inverse operation to scaleOne
  let scaleOneInv (tlv:float<'u>, thv:float<'u>) (scale:Scale<'v>) (coord:continuous<'u>) : continuous<'v> =  
    match scale, coord with
    | Continuous(CO slv, CO shv), (CO v) ->
        CO((v - tlv) / (thv - tlv) * (shv - slv) + slv)

   // project:    scales<v> * point<v> * proj<v -> u> -> point<u>   // v = -1 .. 1     u = 0px .. 100px
   // projectInv: scales<v> * point<u> * proj<v -> u> -> point<v>

  let rec projectInv<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> 
      ((sx, sy):Scale<'vx> * Scale<'vy>) (point:continuous<'ux> * continuous<'uy>) 
      (projection:Projection<'vx, 'vy, 'ux, 'uy>) : continuous<'vx> * continuous<'vy> = 
    
    match projection, point with
    | Padding(t,r,b,l,projection), (CO x, CO y) ->
        let (Continuous(lx, hx), Continuous(ly, hy)) = sx, sy
        let (CO x1, CO y1) = project sx sy (lx, ly) projection
        let (CO x2, CO y2) = project sx sy (hx, hy) projection
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2
        
        // Imagine point is in 20px .. 60px, calculate equivalent point in 0px .. 100px (add padding)
        let (CO ox, CO oy) = point 
        let nx = (ox - l) / (hx - lx - l - r) * (hx - lx)
        let ny = (oy - t) / (hy - ly - t - b) * (hy - ly)
        projectInv (sx, sy) (CO nx, CO ny) projection

    | Scale(tx, ty), (x, y) ->
        scaleOneInv tx sx x, scaleOneInv ty sy y 

  let projectEvent scales projection event =
    match event with
    | MouseEvent(kind, (x, y)) -> MouseEvent(kind, projectInv scales (x, y) projection)
    | TouchEvent(kind, (x, y)) -> TouchEvent(kind, projectInv scales (x, y) projection)
    | MouseLeave -> MouseLeave

  let inScales (Continuous(CO x1, CO x2), Continuous(CO y1, CO y2)) event =
    match event with
    | MouseLeave -> true
    | MouseEvent(_, (CO x, CO y)) 
    | TouchEvent(_, (CO x, CO y)) -> 
        x > min x1 x2 && x < max x1 x2 &&
          y > min y1 y2 && y < max y1 y2 

  let rec triggerEvent<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ProjectedShape<'ux, 'uy>) (jse:Event) (event:InteractiveEvent<1,1>) = 
    let (Projected(projection, scales, shape)) = shape
    match shape with
    | ProjectedLine _
    | ProjectedText _
    | ProjectedArea _ -> ()
    | ProjectedStyle(_, shape)
    | ProjectedAxes(_, _, _, shape) -> triggerEvent shape jse event
    | ProjectedLayered shapes -> for shape in shapes do triggerEvent shape jse event
    | ProjectedInteractive(handlers, shape) ->
        let localEvent = projectEvent scales projection event
        if inScales scales localEvent then 
          for handler in handlers do 
            match localEvent, handler with
            | MouseEvent(MouseEventKind.Move, pt), MouseMove(f) 
            | MouseEvent(MouseEventKind.Up, pt), MouseUp(f) 
            | MouseEvent(MouseEventKind.Down, pt), MouseDown(f) -> 
                jse.preventDefault()
                f (unbox jse) pt
            | TouchEvent(TouchEventKind.Move, pt), TouchMove(f) 
            | TouchEvent(TouchEventKind.Start, pt), TouchStart(f) 
            | TouchEvent(TouchEventKind.End, pt), TouchEnd(f) -> 
                jse.preventDefault()
                f (unbox jse) pt
            | MouseLeave, EventHandler.MouseLeave f -> f (unbox jse) 
            | MouseLeave, _ 
            | TouchEvent(_, _), _  
            | MouseEvent(_, _), _  -> ()

        triggerEvent shape jse event

  // ----------------------------------------------------------------------------------------------
  // EOF
  // ----------------------------------------------------------------------------------------------

module InteractiveHelpers = 

  let app id initial r u = 
    let event = new Event<'T>()
    let trigger e = event.Trigger(e)  
    let mutable container = document.createElement("div") :> Node
    document.getElementById(id).innerHTML <- ""
    document.getElementById(id).appendChild(container) |> ignore
    let mutable tree = Fable.Core.JsInterop.createObj []
    let mutable state = initial

    let handleEvent evt = 
      state <- match evt with Some e -> u state e | _ -> state
      let newTree = r trigger state |> renderVirtual
      let patches = Virtualdom.diff tree newTree
      container <- Virtualdom.patch container patches
      tree <- newTree
  
    handleEvent None
    event.Publish.Add(Some >> handleEvent)

  open Visualizations

  type YouDrawEvent = 
    | ShowResults
    | Draw of float * float

  type YouDrawState = 
    { Completed : bool
      Clip : float
      Data : (float * float)[]
      Guessed : (float * option<float>)[] }

  let initState data clipx = 
    { Completed = false
      Data = data
      Clip = clipx
      Guessed = [| for x, y in data do if x > clipx then yield x, None |] }

  let handler state evt = 
    match evt with
    | ShowResults -> { state with Completed = true }
    | Draw (downX, downY) ->
        let indexed = Array.indexed state.Guessed
        let nearest, _ = indexed |> Array.minBy (fun (_, (x, _)) -> abs (downX - x))
        { state with
            Guessed = indexed |> Array.map (fun (i, (x, y)) -> 
              if i = nearest then (x, Some downY) else (x, y)) }

  let render (width, height) (topLbl, leftLbl, rightLbl) (leftClr,rightClr,guessClr) (loy, hiy) trigger state = 
    let all = 
      [| for x, y in state.Data -> CO x, CO y |]
    let known = 
      [| for x, y in state.Data do if x <= state.Clip then yield CO x, CO y |]
    let right = 
      [| yield Array.last known
         for x, y in state.Data do if x > state.Clip then yield CO x, CO y |]
    let guessed = 
      [| yield Array.last known
         for x, y in state.Guessed do if y.IsSome then yield CO x, CO y.Value |]

    let lx, ly = (fst (Seq.head state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let rx, ry = (fst (Seq.last state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let tx, ty = float state.Clip, hiy - (hiy - loy) / 10.
    let setColor c s = { s with Font = "12pt sans-serif"; Fill=Solid(1.0, HTML c); StrokeColor=(0.0, RGB(0,0,0)) }
    let labels = 
      Shape.Layered [
        Style(setColor leftClr, Shape.Text(CO lx, CO ly, VerticalAlign.Baseline, HorizontalAlign.Center, leftLbl))
        Style(setColor rightClr, Shape.Text(CO rx, CO ry, VerticalAlign.Baseline, HorizontalAlign.Center, rightLbl))
        Style(setColor guessClr, Shape.Text(CO tx, CO ty, VerticalAlign.Baseline, HorizontalAlign.Center, topLbl))
      ]

    let chart = 
      Axes
        (Interactive(
          ( if state.Completed then []
            else
              [ MouseMove(fun evt (CO x, CO y) -> 
                  if (int evt.buttons) &&& 1 = 1 then trigger(Draw(x, y)) )
                TouchMove(fun evt (CO x, CO y) -> 
                  trigger(Draw(x, y)) )
                MouseDown(fun evt (CO x, CO y) -> trigger(Draw(x, y)) )
                TouchStart(fun evt (CO x, CO y) -> trigger(Draw(x, y)) ) ]),
          Shape.Scale
            ( None, Some(CO loy, CO hiy), 
              Layered [
                yield labels
                yield Style(hideFill >> hideStroke, Line all)
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
                    Line guessed ) 
              ])
        ))

    let scaled = calculateScales chart
    let master = Scale((0.0, width), (height, 0.0))
    let projected = calculateProjections scaled master
    let defstyle = 
      { Fill = Solid(1.0, RGB(196, 196, 196))
        StrokeColor = (1.0, RGB(256, 0, 0))
        StrokeDashArray = []
        StrokeWidth = Pixels 2
        Animation = None 
        Font = "10pt sans-serif" }
    let defs = ResizeArray<_>()
    let svg = drawShape defs defstyle projected

    let getRelativeLocation el x y =
      let rec getOffset (parent:HTMLElement) (x, y) = 
        if parent = null then (x, y)
        else getOffset (unbox parent.offsetParent) (x-parent.offsetLeft, y-parent.offsetTop)
      let rec getParent (parent:HTMLElement) = 
        if parent.offsetParent <> null then parent 
        else getParent parent.parentElement
      getOffset (getParent el) (x, y)
    
    let mouseHandler kind el (evt:Event) =
      let evt = evt :?> MouseEvent
      let x, y = getRelativeLocation el evt.pageX evt.pageY
      triggerEvent projected evt (MouseEvent(kind, (CO x, CO y)))

    let touchHandler kind el (evt:Event) =
      let evt = evt :?> TouchEvent
      let touch = evt.touches.[0]
      let x, y = getRelativeLocation el touch.pageX touch.pageY
      triggerEvent projected evt (TouchEvent(kind, (CO x, CO y)))

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      h?div ["style"=>sprintf "width:%dpx;height:%dpx;margin:0px auto 0px auto" (int width) (int height)] [
        s?svg [
            "width"=>string (int width); "height"=> string(int height); 
            "mousemove" =!> mouseHandler MouseEventKind.Move
            "mousedown" =!> mouseHandler MouseEventKind.Down
            "mouseup" =!> mouseHandler MouseEventKind.Up
            "mouseleave" =!> fun _ evt -> triggerEvent projected evt MouseLeave
            "touchmove" =!> touchHandler TouchEventKind.Move
            "touchdown" =!> touchHandler TouchEventKind.Start
            "touchup" =!> touchHandler TouchEventKind.End
          ] [
            yield! defs
            yield! renderSvg svg
          ]
      ]
      h?div ["style"=>"padding-bottom:20px"] [
        h?button [
            yield "click" =!> fun _ _ -> trigger ShowResults
            if state.Guessed |> Seq.last |> snd = None then
              yield "disabled" => "disabled"
          ] [ text "Show me how I did" ]
        ]
    ]

open TheGamma.Series
open TheGamma.Common

type youdraw = 
  { data : series<float, float> 
    clip : float option
    min : float option
    max : float option 
    knownColor : string option
    unknownColor : string option 
    drawColor : string option 
    topLabel : string option
    knownLabel : string option
    guessLabel : string option }
  static member create(data:series<float, float>) =
    { youdraw.data = data
      clip = None; min = None; max = None 
      guessLabel = None; topLabel = None; knownLabel = None;
      knownColor = None; unknownColor = None; drawColor = None }
  member y.setRange(min, max) = { y with min = Some min; max = Some max }
  member y.setClip(clip) = { y with clip = Some clip }
  member y.setColors(known, unknown) = { y with knownColor = Some known; unknownColor = Some unknown }
  member y.setDrawColor(draw) = { y with drawColor = Some draw }
  member y.setLabels(top, known, guess) = { y with knownLabel = Some known; topLabel = Some top; guessLabel = Some guess }
  member y.show(outputId) =   
    async { 
      let id = "container" + System.Guid.NewGuid().ToString().Replace("-", "")
      h?div ["id" => id] [ ] |> renderTo (document.getElementById(outputId))        

      // Get data & wait until the element is created
      let! data = y.data.data |> Async.AwaitFuture 
      let mutable i = 10
      while i > 0 && document.getElementById(id) = null do
        do! Async.Sleep(10)
        i <- i - 1
      let element = document.getElementById(id)
      let size = element.clientWidth, max 400. (element.clientWidth / 2.) 

      try
        let loy = match y.min with Some v -> v | _ -> data |> Seq.map snd |> Seq.min
        let hiy = match y.max with Some v -> v | _ -> data |> Seq.map snd |> Seq.max
        let clipx = match y.clip with Some v -> v | _ -> fst (data.[data.Length / 2])
        let data = Array.sortBy fst data
        let lc, dc, gc = defaultArg y.knownColor "#606060", defaultArg y.unknownColor "#FFC700", defaultArg y.drawColor "#808080"          
        InteractiveHelpers.app outputId 
          (InteractiveHelpers.initState data clipx) 
          (InteractiveHelpers.render size
            (defaultArg y.topLabel "", defaultArg y.knownLabel "", defaultArg y.guessLabel "") 
            (lc,dc,gc) (loy, hiy)) InteractiveHelpers.handler
      with e ->
        Log.exn("GUI", "Interactive rendering failed: %O", e)
    } |> Async.StartImmediate  
