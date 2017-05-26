namespace TheGamma.Interactive.Compost

open TheGamma.Html
open Fable.Import.Browser
open Fable.Helpers

// ------------------------------------------------------------------------------------------------
// Domain that users see
// ------------------------------------------------------------------------------------------------
  
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
    Font : string
    Cursor : string }

type HorizontalAlign = Start | Center | End
type VerticalAlign = Baseline | Middle | Hanging

type continuous<[<Measure>] 'u> = CO of float<'u> 
type categorical<[<Measure>] 'u> = CA of string

type Value<[<Measure>] 'u> = 
  | CAR of categorical<'u> * float
  | COV of continuous<'u>

type Scale<[<Measure>] 'v> =
  | Continuous of continuous<'v> * continuous<'v>
  | Categorical of categorical<'v>[]

type EventHandler<[<Measure>] 'vx, [<Measure>] 'vy> = 
  | MouseMove of (MouseEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | MouseUp of (MouseEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | MouseDown of (MouseEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | Click of (MouseEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | TouchStart of (TouchEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | TouchEnd of (TouchEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | TouchMove of (TouchEvent -> (Value<'vx> * Value<'vy>) -> unit)
  | MouseLeave of (MouseEvent -> unit)

type Orientation = 
  | Vertical
  | Horizontal

type Shape<[<Measure>] 'vx, [<Measure>] 'vy> = 
  | Style of (Style -> Style) * Shape<'vx, 'vy>
  | Text of Value<'vx> * Value<'vy> * VerticalAlign * HorizontalAlign * string
  | AutoScale of (bool * bool) * Shape<'vx, 'vy>
  | InnerScale of option<continuous<'vx> * continuous<'vx>> * option<continuous<'vy> * continuous<'vy>> * Shape<'vx, 'vy>
  | OuterScale of option<Scale<'vx>> * option<Scale<'vy>> * Shape<'vx, 'vy>
  | Line of seq<Value<'vx> * Value<'vy>>
  | Area of seq<Value<'vx> * Value<'vy>>
  | Bar of continuous<'vx> * categorical<'vy>
  | Column of categorical<'vx> * continuous<'vy>
  | Stack of Orientation * seq<Shape<'vx, 'vy>>
  | Layered of seq<Shape<'vx, 'vy>>
  | Axes of bool * bool * Shape<'vx, 'vy>
  | Interactive of seq<EventHandler<'vx, 'vy>> * Shape<'vx, 'vy>
  | Padding of (float * float * float * float) * Shape<'vx, 'vy>
  | Offset of (float * float) * Shape<'vx, 'vy>

// ------------------------------------------------------------------------------------------------
// SVG stuff
// ------------------------------------------------------------------------------------------------

module Svg =   

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

  let rec mapSvg f = function
    | Combine svgs -> Combine(Array.map (mapSvg f) svgs)
    | svg -> f svg

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
    ( "cursor:" + style.Cursor + ";") +
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

// ------------------------------------------------------------------------------------------------
// Calculating scales
// ------------------------------------------------------------------------------------------------

module Scales = 
  type ScaledShapeInner<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | ScaledStyle of (Style -> Style) * ScaledShape<'vx, 'vy>
    | ScaledOuterScale of option<Scale<'vx>> * option<Scale<'vy>> * ScaledShape<'vx, 'vy>
    | ScaledText of Value<'vx> * Value<'vy> * VerticalAlign * HorizontalAlign * string    
    | ScaledLine of (Value<'vx> * Value<'vy>)[]
    | ScaledArea of (Value<'vx> * Value<'vy>)[]
    | ScaledLayered of ScaledShape<'vx, 'vy>[]
    | ScaledColumn of categorical<'vx> * continuous<'vy>
    | ScaledBar of continuous<'vx> * categorical<'vy>
    | ScaledStack of Orientation * ScaledShape<'vx, 'vy>[]
    | ScaledInteractive of seq<EventHandler<'vx, 'vy>> * ScaledShape<'vx, 'vy>
    | ScaledPadding of (float * float * float * float) * ScaledShape<'vx, 'vy>
    | ScaledOffset of (float * float) * ScaledShape<'vx, 'vy>

  and ScaledShape<[<Measure>] 'vx, [<Measure>] 'vy> =
    Scaled of outer:(Scale<'vx> * Scale<'vy>) * inner:(Scale<'vx> * Scale<'vy>) * ScaledShapeInner<'vx, 'vy>

  let niceNumber num decs =
    let str = string num
    let dot = str.IndexOf('.')
    let before, after = 
      if dot = -1 then str, ""
      else str.Substring(0, dot), str.Substring(dot, min (decs + 1) (str.Length - dot))
    let mutable res = before
    if before.Length > 5 then
      for i in before.Length-1 .. -1 .. 0 do
        let j = before.Length - i
        if i <> 0 && j % 3 = 0 then res <- res.Insert(i, ",")
    res + after

  let getExtremes = function
    | Continuous(l, h) -> COV l, COV h
    | Categorical(vals) ->  CAR(vals.[0], 0.0), CAR(vals.[vals.Length-1], 1.0)

  /// Given a range, return a new aligned range together with the magnitude  
  /// (that is 10^n such that it fits in lo .. hi less than 10 times)
  let calculateMagnitudeAndRange (lo:float, hi:float) = 
    let magnitude = 10. ** round (log10 (hi - lo))
    let magnitude = magnitude / 2.
    magnitude, (floor (lo / magnitude) * magnitude, ceil (hi / magnitude) * magnitude)

  /// Get number of decimal points to show for the given range
  let decimalPoints range = 
    let magnitude, _ = calculateMagnitudeAndRange range
    max 0. (-(log10 (magnitude * 2.0)))

  /// Extend the given range to a nicely adjusted size
  let adjustRange range = snd (calculateMagnitudeAndRange range)
  let adjustRangeUnits (l:float<'u>,h:float<'u>) : float<'u> * float<'u> =
    let l, h = adjustRange (unbox l, unbox h) in unbox l, unbox h

  /// Generate points for a grid. Count specifies how many points to generate
  /// (this is minimm - the result will be up to 5x more).
  let generateSteps count k (lo, hi) = 
    let magnitude, (nlo, nhi) = calculateMagnitudeAndRange (lo, hi)
    let dividers = [0.2; 0.5; 1.; 2.; 5.; 10.; 20.; 40.; 50.; 60.; 80.; 100.]
    let magnitudes = dividers |> Seq.map (fun d -> magnitude / d)
    let step = magnitudes |> Seq.filter (fun m -> (hi - lo) / m >= count) |> Seq.tryHead
    let step = defaultArg step (magnitude / 100.)
    [| for v in nlo .. step * k .. nhi do
          if v >= lo && v <= hi then yield v |]

  let generateAxisSteps s =
    match s with 
    | Continuous(CO l, CO h) ->
        generateSteps 6. 1. (float l, float h) |> Array.map (fun f -> COV(CO (unbox f)))
    | Categorical vs -> [| for CA s in vs -> CAR(CA s, 0.5) |]

  let generateAxisLabels (s:Scale<'v>) : (Value<'v> * string)[] =
    match s with 
    | Continuous(CO l, CO h) ->
        let dec = decimalPoints (unbox l, unbox h)
        generateSteps 6. 2. (float l, float h) |> Array.map (fun f -> COV(CO (unbox f)), niceNumber (unbox f) (int dec))
    | Categorical vs -> [| for CA s in vs -> CAR(CA s, 0.5), s |]

  let unionScales s1 s2 =
    match s1, s2 with
    | Continuous(l1, h1), Continuous(l2, h2) -> Continuous(min l1 l2, max h1 h2)
    | Categorical(v1), Categorical(v2) -> Categorical(Array.distinct (Array.append v1 v2))
    | _ -> 
        failwith "Cannot union continuous with categorical"

  // Replace scales in all immediately nested things that will
  // share the same scale when combined via Layered
  // (recursively over Interacitve & Layered with Line as leaf)

  let rec replaceScales outer (Scaled(_, inner, shape) as scaled) =
    match shape with
    // Replace at the leafs
    | ScaledLine _ 
    | ScaledText _
    | ScaledColumn _
    | ScaledBar _    
    | ScaledArea _ -> Scaled(outer, inner, shape)
    // Replace just top level scales
    | ScaledOuterScale _ -> Scaled(outer, inner, shape)
    // Propagate recursively
    | ScaledOffset(d, shape) -> Scaled(outer, inner, ScaledOffset(d, replaceScales outer shape))
    | ScaledStyle(f, shape) -> Scaled(outer, inner, ScaledStyle(f, replaceScales outer shape))
    | ScaledPadding(pad, shape) -> Scaled(outer, inner, ScaledPadding(pad, replaceScales outer shape))
    | ScaledInteractive(f, shape) -> Scaled(outer, inner, ScaledInteractive(f, replaceScales outer shape))
    | ScaledLayered(shapes) -> Scaled(outer, inner, ScaledLayered(Array.map (replaceScales outer) shapes))
    | ScaledStack(orient, shapes) -> Scaled(outer, inner, ScaledStack(orient, Array.map (replaceScales outer) shapes))

  // From the leafs to the root, calculate the scales of
  // everything (composing sales of leafs to get scale of root)

  let calculateLineOrAreaScale vals = 
    let scales =
      vals |> Array.fold (fun state value ->
        match state, value with 
        | Choice1Of3(), COV(CO v) -> Choice2Of3([v])
        | Choice2Of3(vs), COV(CO v) -> Choice2Of3(v::vs)
        | Choice1Of3(), CAR(CA x, _) -> Choice3Of3([x])
        | Choice3Of3(xs), CAR(CA x, _) -> Choice3Of3(x::xs)
        | _ -> failwith "Values with mismatching scales") (Choice1Of3())
    match scales with
    | Choice1Of3() -> failwith "No values for calculating a scale"
    | Choice2Of3(vs) -> Continuous (CO (List.min vs), CO (List.max vs))
    | Choice3Of3(xs) -> Categorical (Array.distinct [| for x in List.rev xs -> CA x |])

  let calculateLineOrAreaScales line = 
    let xs = line |> Array.map fst 
    let ys = line |> Array.map snd
    calculateLineOrAreaScale xs, calculateLineOrAreaScale ys
(*
  let calculateAreaScales line = 
    let xs = line |> Array.map fst 
    let ys = line |> Array.map snd
    let x0, x1 = Array.min xs, Array.max xs
    let y0, y1 = Array.min ys, Array.max ys
    Continuous(x0, x1), Continuous(y0, y1)
*)
  // Always returns objects with the same inner and outer scales
  // but outer scales can be replaced later by replaceScales
  let rec calculateScales<[<Measure>] 'ux, [<Measure>] 'uy> (shape:Shape<'ux, 'uy>) = 
    match shape with
    | OuterScale(sx, sy, shape) ->
        let (Scaled((osx, osy), inner, _)) as scaled = calculateScales shape
        let scales = defaultArg sx osx, defaultArg sy osy
        Scaled(scales, inner, ScaledOuterScale(sx, sy, scaled))

    | InnerScale(sx, sy, shape) ->
        let (Scaled((asx, asy), _, shape)) = calculateScales shape
        let scales = 
          (match sx with Some sx -> Continuous(sx) | _ -> asx), 
          (match sy with Some sy -> Continuous(sy) | _ -> asy) 
        Scaled(scales, scales, shape) |> replaceScales scales

    | AutoScale((ax, ay), shape) ->
        let (Scaled((asx, asy), _, shape)) = calculateScales shape
        let autoScale = function
          | Continuous(CO l, CO h) -> let l, h = adjustRangeUnits (l, h) in Continuous(CO l, CO h)
          | scale -> scale
        let scales = 
          ( if ax then autoScale asx else asx ),
          ( if ay then autoScale asy else asy )
        Scaled(scales, scales, shape) |> replaceScales scales    

    | Offset(offs, shape) ->
        let (Scaled(scales, _, shape)) = calculateScales shape
        Scaled(scales, scales, ScaledOffset(offs, Scaled(scales, scales, shape)))

    | Style(style, shape) ->
        let (Scaled(scales, _, shape)) = calculateScales shape
        Scaled(scales, scales, ScaledStyle(style, Scaled(scales, scales, shape)))

    | Padding(pads, shape) ->
        let (Scaled(scales, _, shape)) = calculateScales shape
        Scaled(scales, scales, ScaledPadding(pads, Scaled(scales, scales, shape)))

    | Column(x, y) ->
        let scales = Categorical [| x |], Continuous(CO 0.0<_>, y)
        Scaled(scales, scales, ScaledColumn(x, y))

    | Bar(x, y) ->
        let scales = Continuous(CO 0.0<_>, x), Categorical [| y |]
        Scaled(scales, scales, ScaledBar(x, y))

    | Shape.Text(x, y, va, ha, t) ->
        let makeSingletonScale = function COV(v) -> Continuous(v, v) | CAR(v, _) -> Categorical [| v |]
        let scales = makeSingletonScale x, makeSingletonScale y
        Scaled(scales, scales, ScaledText(x, y, va, ha, t))    

    | Line line -> 
        let line = Seq.toArray line 
        let scales = calculateLineOrAreaScales line
        Scaled(scales, scales, ScaledLine(line))

    | Area area -> 
        let area = Seq.toArray area
        let scales = calculateLineOrAreaScales area
        Scaled(scales, scales, ScaledArea(area))
    
    | Axes(showX, showY, shape) ->
        let (Scaled(origScales & (sx, sy), _, _)) = calculateScales shape 
        let (lx, hx), (ly, hy) = getExtremes sx, getExtremes sy
        
        let LineStyle clr alpha width shape = 
          Style((fun s -> { s with Fill = Solid(1.0, HTML "transparent"); StrokeWidth = Pixels width; StrokeColor=alpha, HTML clr }), shape)
        let FontStyle style shape = 
          Style((fun s -> { s with Font = style; Fill = Solid(1.0, HTML "black"); StrokeColor = 0.0, HTML "transparent" }), shape)

        let shape = 
          Layered [ 
            for x in generateAxisSteps sx do
              yield Line [x,ly; x,hy] |> LineStyle "#e4e4e4" 1.0 1
            for y in generateAxisSteps sy do
              yield Line [lx,y; hx,y] |> LineStyle "#e4e4e4" 1.0 1 
            yield Line [lx,hy; lx,ly; hx,ly] |> LineStyle "black" 1.0 2
            if showX then
              for x, l in generateAxisLabels sx do
                yield Offset((0., 10.), Text(x, ly, VerticalAlign.Hanging, HorizontalAlign.Center, l)) |> FontStyle "9pt sans-serif"
            if showY then
              for y, l in generateAxisLabels sy do
                yield Offset((-10., 0.), Text(lx, y, VerticalAlign.Middle, HorizontalAlign.End, l)) |> FontStyle "9pt sans-serif"
            yield shape ] |> calculateScales

        match shape with 
        | Scaled(_, _, ScaledLayered(shapes)) ->
            let padding = (10., 20., (if showX then 40. else 20.), (if showY then 100. else 20.))
            Scaled(origScales, origScales, 
              ScaledPadding(padding, 
                Scaled(origScales, origScales, 
                  ScaledLayered (Array.map (replaceScales origScales) shapes))))
        | _ -> failwith "calculateScales: processing layered shape did not return layered shape"
        
    | Stack(orient, shapes) ->
        let shapes = shapes |> Array.ofSeq
        let scaled = shapes |> Array.map calculateScales 
        let sxs = scaled |> Array.map (fun (Scaled((sx, _), _, _)) -> sx)
        let sys = scaled |> Array.map (fun (Scaled((_, sy), _, _)) -> sy)
        let scales = (Array.reduce unionScales sxs, Array.reduce unionScales sys)
        match orient, scales with 
        | Horizontal, (Continuous _, _) -> failwith "Horizontal stacking of continuous axes is not supported"
        | Vertical, (_, Continuous _) -> failwith "Vertical stacking of continuous axes is not supported"
        | _ -> ()
        Scaled(scales, scales, ScaledStack(orient, scaled)) |> replaceScales scales 

    | Layered shapes ->
        let shapes = shapes |> Array.ofSeq
        let scaled = shapes |> Array.map calculateScales 
        let sxs = scaled |> Array.map (fun (Scaled((sx, _), _, _)) -> sx)
        let sys = scaled |> Array.map (fun (Scaled((_, sy), _, _)) -> sy)
        let scales = (Array.reduce unionScales sxs, Array.reduce unionScales sys)
        Scaled(scales, scales, ScaledLayered scaled) |> replaceScales scales 

    | Interactive(f, shape) ->
        let (Scaled(scales, _, shape)) = calculateScales shape
        Scaled(scales, scales, ScaledInteractive(f, Scaled(scales, scales, shape)))

// ------------------------------------------------------------------------------------------------
// Calculate projections
// ------------------------------------------------------------------------------------------------

module Projections = 
  open Scales

  type Projection<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> = 
    | Scale of (float<'ux> * float<'ux>) * (float<'uy> * float<'uy>)
    
    // given a projection that maps things to (0, 100), the floats
    // specify subrange of the target domain, so i.e. specifying (0.2, 0.8) would
    // result in all the vales being mapped to (20, 80)
    | Rescale of (float * float) * (float * float) * Projection<'vx, 'vy, 'ux, 'uy> 

    | Padding of
        // padding from top, right, bottom, left 
        padding:(float<'uy> * float<'ux> * float<'uy> * float<'ux>) * 
        // points on the scales relative to which the padding is calculated
        // (essentially the size of the content around which padding is)
        extremes:(Value<'vx> * Value<'vx> * Value<'vy> * Value<'vy>) * 
        Projection<'vx, 'vy, 'ux, 'uy>

  type ProjectedShapeInner<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | ProjectedStyle of (Style -> Style) * ProjectedShape<'vx, 'vy>
    | ProjectedText of Value<'vx> * Value<'vy> * VerticalAlign * HorizontalAlign * string    
    | ProjectedLine of (Value<'vx> * Value<'vy>)[]
    | ProjectedArea of (Value<'vx> * Value<'vy>)[]
    | ProjectedLayered of ProjectedShape<'vx, 'vy>[]
    | ProjectedColumn of categorical<'vx> * continuous<'vy>
    | ProjectedBar of continuous<'vx> * categorical<'vy>
    | ProjectedStack of Orientation * ProjectedShape<'vx, 'vy>[]
    | ProjectedOffset of (float * float) * ProjectedShape<'vx, 'vy>
    | ProjectedInteractive of seq<EventHandler<'vx, 'vy>> * ProjectedShape<'vx, 'vy>

  /// Projection from values on the scales (specified) to pixels
  and ProjectedShape<[<Measure>] 'vx, [<Measure>] 'vy> =
    Projected of Projection<'vx, 'vy, 1, 1> * (Scale<'vx> * Scale<'vy>) * ProjectedShapeInner<'vx, 'vy>

  let scaleOne (tlv:float<_>, thv:float<_>) scale coord = 
    match scale, coord with
    | Categorical(vals), (CAR(CA v,f)) ->
        let size = (thv - tlv) / float vals.Length
        let i = vals |> Array.findIndex (fun (CA vv) -> v = vv)
        let i = float i + f
        CO(tlv + (i * size))
    | Continuous(CO slv, CO shv), (COV (CO v)) ->
        CO((v - slv) / (shv - slv) * (thv - tlv) + tlv)
    | Categorical _, COV _ -> failwith "Cannot project continuous value on a categorical scale."
    | Continuous _, CAR _ -> failwith "Cannot project categorical value on a continuous scale."

  let rec project<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> 
      (sx:Scale<'vx>) (sy:Scale<'vy>) point (projection:Projection<'vx, 'vy, 'ux, 'uy>) : continuous<'ux> * continuous<'uy> = 
    match projection, point with
    | Scale(tx, ty), (x, y) ->
        scaleOne tx sx x, scaleOne ty sy y 
    
    | Rescale((rlx, rhx), (rly, rhy), proj), point ->
        let (lx, hx), (ly, hy) = getExtremes sx, getExtremes sy
        let (CO x1, CO y1), (CO x2, CO y2) = project sx sy (lx, ly) proj, project sx sy (hx, hy) proj
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2

        let (CO x, CO y) = project sx sy point proj 
        let nx = if lx = hx then x else lx + (hx - lx) * ((x - lx) / (hx - lx) * (rhx - rlx) + rlx)
        let ny = if ly = hy then y else ly + (hy - ly) * ((y - ly) / (hy - ly) * (rhy - rly) + rly)
        (CO nx, CO ny)

    | Padding((t,r,b,l),(lx,hx,ly,hy),projection), _ ->
        //let (lx, hx), (ly, hy) = getExtremes sx, getExtremes sy
        let (CO x1, CO y1) = project sx sy (lx, ly) projection
        let (CO x2, CO y2) = project sx sy (hx, hy) projection
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2

        let (CO x, CO y) = project sx sy point projection

        // Assuming the result is in pixels...
        let nx = if lx = hx then x else lx + l + (hx - lx - l - r) / (hx - lx) * (x - lx)
        let ny = if ly = hy then y else ly + t + (hy - ly - t - b) / (hy - ly) * (y - ly)
        (CO nx, CO ny)
    

  // inverse operation to scaleOne
  let scaleOneInv (tlv:float<'u>, thv:float<'u>) (scale:Scale<'v>) (coord:continuous<'u>) : Value<'v> =  
    match scale, coord with
    | Continuous(CO slv, CO shv), (CO v) ->
        COV (CO((v - tlv) / (thv - tlv) * (shv - slv) + slv))
    | Categorical(cats), (CO v) ->
        let size = (thv - tlv) / float cats.Length
        let i = floor (v / size)
        let f = (v / size) - i
        let i = if size < 0.<_> then (float cats.Length) + i else i // Negative when thv < tlv
        if int i < 0 || int i >= cats.Length then CAR(CA "<outside-of-range>", f)
        else CAR(cats.[int i], f)

   // project:    scales<v> * point<v> * proj<v -> u> -> point<u>   // v = -1 .. 1     u = 0px .. 100px
   // projectInv: scales<v> * point<u> * proj<v -> u> -> point<v>

  let rec invertProj proj = 
    match proj with
    | Rescale(rx, ry, Padding(p, ex, proj)) -> Padding(p, ex, Rescale(rx, ry, proj))
    | Padding(p, ex, Rescale(rx, ry, proj)) -> Rescale(rx, ry, Padding(p, ex, proj))
    | _ -> proj


  let rec projectInv<[<Measure>] 'vx, [<Measure>] 'vy, [<Measure>] 'ux, [<Measure>] 'uy> 
      ((sx, sy):Scale<'vx> * Scale<'vy>) (point:continuous<'ux> * continuous<'uy>) 
      (projection:Projection<'vx, 'vy, 'ux, 'uy>) : Value<'vx> * Value<'vy> = 
    
    match projection, point with
    | Rescale((rlx, rhx), (rly, rhy), projection), (CO x, CO y) ->
        let (lx, hx), (ly, hy) = getExtremes sx, getExtremes sy
        let (CO x1, CO y1), (CO x2, CO y2) = project sx sy (lx, ly) projection, project sx sy (hx, hy) projection
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2

        // Inverse of project
        let (CO ox, CO oy) = point 
        let nx = lx + ((ox - lx) / (hx - lx) - rlx) / (rhx - rlx) * (hx - lx)
        let ny = ly + ((oy - ly) / (hy - ly) - rly) / (rhy - rly) * (hy - ly)
        projectInv (sx, sy) (CO nx, CO ny) projection

    | Padding((t, r, b, l), (lx, hx, ly, hy), projection), (CO x, CO y) ->
        let (CO x1, CO y1) = project sx sy (lx, ly) projection
        let (CO x2, CO y2) = project sx sy (hx, hy) projection
        let lx, hx, ly, hy = min x1 x2, max x1 x2, min y1 y2, max y1 y2
        
        // Imagine point is in 20px .. 60px, calculate equivalent point in 0px .. 100px (add padding)
        let (CO ox, CO oy) = point 
        //let nx = (ox - l) / (hx - lx - l - r) * (hx - lx)
        //let ny = (oy - t) / (hy - ly - t - b) * (hy - ly)
        let nx = (ox - lx - l) / (hx - lx - l - r) * (hx - lx) + lx
        let ny = (oy - ly - t) / (hy - ly - t - b) * (hy - ly) + ly
        projectInv (sx, sy) (CO nx, CO ny) projection

    | Scale(tx, ty), (x, y) ->
        scaleOneInv tx sx x, scaleOneInv ty sy y 


  let rec calculateProjections<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ScaledShape<'ux, 'uy>) projection = 
    match shape with
    | Scaled(scales, _, ScaledOffset(offs, shape)) ->
        Projected(projection, scales, ProjectedOffset(offs, calculateProjections shape projection))

    | Scaled(scales, _, ScaledStyle(style, shape)) ->
        Projected(projection, scales, ProjectedStyle(style, calculateProjections shape projection))

    | Scaled((sx, sy), _, ScaledOuterScale(osx, osy, shape)) ->
        //let pinner = Projection

        // projection + shape scales determines mapping from shape scales to pixel space
        // get range of osx/osy within scales and transform projection so that it only maps on this subrange
        let projection = 
          match osy with
          | Some(oy) ->
              let lsy, hsy = getExtremes sy
              let lsy', hsy' = scaleOne (0.0, 1.0) sy lsy, scaleOne (0.0, 1.0) sy hsy
              let loy, hoy = getExtremes oy
              let (CO loy'), (CO hoy') = scaleOne (0.0, 1.0) sy loy, scaleOne (0.0, 1.0) sy hoy
              Rescale((0.0, 1.0), (loy', hoy'), projection)
          | _ -> 
              projection

        let (Projected(pbody, scales, nested)) = calculateProjections shape projection
        //Projected(projection, scales, ProjectedStyle(style, ))
        //failwith "hard!"
        Projected(pbody, scales, nested)

    | Scaled(scales, _, ScaledLine line) -> 
        Projected(projection, scales, ProjectedLine line)

    | Scaled(scales, _, ScaledBar(x,y)) -> 
        Projected(projection, scales, ProjectedBar(x, y))

    | Scaled(scales, _, ScaledColumn(x,y)) -> 
        Projected(projection, scales, ProjectedColumn(x, y))

    | Scaled(scales, _, ScaledText(x, y, va, ha, t)) -> 
        Projected(projection, scales, ProjectedText(x, y, va, ha, t))

    | Scaled(scales, _, ScaledArea area) -> 
        Projected(projection, scales, ProjectedArea area)

    | Scaled(_, _, ScaledPadding((t,r,b,l), shape)) ->
        let (lx, hx), (ly, hy) = 
          let (Scaled(_, (sxinner, syinner), _)) = shape 
          getExtremes sxinner, getExtremes syinner
        let ppad = Padding((t, r, b, l), (lx, hx, ly, hy), projection)
        console.log("PPAD = %O", ppad)
        calculateProjections shape ppad

    | Scaled(scales, _, ScaledStack(orient, shapes)) ->
        Projected(projection, scales, ProjectedStack(orient, shapes |> Array.map (fun s -> calculateProjections s projection)))
        
    | Scaled(scales, _, ScaledLayered shapes) ->
        Projected(projection, scales, ProjectedLayered(shapes |> Array.map (fun s -> calculateProjections s projection)))

    | Scaled(scales, _, ScaledInteractive(f, shape)) ->
        Projected(projection, scales, ProjectedInteractive(f, calculateProjections shape projection))

// ------------------------------------------------------------------------------------------------
// Drawing
// ------------------------------------------------------------------------------------------------

module Drawing = 
  open Svg
  open Scales
  open Projections

  let rec hideFill style = 
    { style with Fill = Solid(0.0, RGB(0, 0, 0)); Animation = match style.Animation with Some(n,e,f) -> Some(n,e,f >> hideFill) | _ -> None }
  let rec hideStroke style = 
    { style with StrokeColor = (0.0, snd style.StrokeColor); Animation = match style.Animation with Some(n,e,f) -> Some(n,e,f >> hideStroke) | _ -> None }

  let rec drawShape<[<Measure>] 'ux, [<Measure>] 'uy> defs style (shape:ProjectedShape<'ux, 'uy>) = 
    let (Projected(projection, (sx, sy), shape)) = shape

    let projectCont (x, y) = 
      match project sx sy (x, y) projection with
      | (CO x), (CO y) -> x, y
    let projectContCov (x, y) = projectCont (COV x, COV y)

    match shape, (sx, sy) with
    | ProjectedOffset((dx, dy), shape), _ ->
        drawShape defs style shape
        |> mapSvg (function 
            | Text((x, y), t, s) -> Text((x + dx, y + dy), t, s)
            | Path(seg, s) -> Path(Array.map (function 
                MoveTo(x, y) -> MoveTo(x + dx, y + dy) | LineTo(x, y) -> LineTo(x + dx, y + dy)) seg, s)
            | s -> s)

    | ProjectedStyle(sf, shape), _ ->
        drawShape defs (sf style) shape

    | ProjectedText(x, y, va, ha, t), _ -> 
        let va = match va with Baseline -> "baseline" | Hanging -> "hanging" | Middle -> "middle"
        let ha = match ha with Start -> "start" | Center -> "middle" | End -> "end"
        Text(projectCont (x, y), t, sprintf "alignment-baseline:%s; text-anchor:%s;" va ha + formatStyle defs style)

    | ProjectedLine line, _ -> 
        let path = 
          [ yield MoveTo(projectCont (Seq.head line)) 
            for pt in Seq.skip 1 line do yield LineTo (projectCont pt) ]
          |> Array.ofList
        Path(path, formatStyle defs (hideFill style)) 

    | ProjectedColumn(x, y), (Continuous _, _)
    | ProjectedColumn(x, y), (_, Categorical _) -> 
        failwith "Column can be drawn only on matching scales"
    | ProjectedColumn(x, y), (Categorical sx, Continuous (ly, hy)) ->
        let path = 
           [| MoveTo(projectCont (CAR(x, 0.0), COV y))
              LineTo(projectCont (CAR(x, 1.0), COV y))
              LineTo(projectCont (CAR(x, 1.0), COV ly))
              LineTo(projectCont (CAR(x, 0.0), COV ly))
              LineTo(projectCont (CAR(x, 0.0), COV y)) |]           
        Path(path, formatStyle defs (hideStroke style)) 

    | ProjectedBar(x, y), (_, Continuous _)
    | ProjectedBar(x, y), (Categorical _, _) -> 
        failwith "Bar can be drawn only on matching scales"
    | ProjectedBar(x, y), (Continuous (lx, hx), Categorical sy) ->
        let path = 
           [| MoveTo(projectCont (COV x, CAR(y, 0.0)))
              LineTo(projectCont (COV x, CAR(y, 1.0)))
              LineTo(projectCont (COV lx, CAR(y, 1.0)))
              LineTo(projectCont (COV lx, CAR(y, 0.0)))
              LineTo(projectCont (COV x, CAR(y, 0.0))) |]           
        Path(path, formatStyle defs (hideStroke style)) 

    | ProjectedArea line, (Categorical _, _)
    | ProjectedArea line, (_, Categorical _) -> 
        failwith "Area can be drawn only on continuous scale"
    | ProjectedArea line, (Continuous(lx, hx), Continuous(ly, hy)) -> 
        let firstX, lastX = fst (Seq.head line), fst (Seq.last line)
        let path = 
          [ yield MoveTo(projectCont (firstX, COV ly))
            for pt in line do yield LineTo (projectCont pt) 
            yield LineTo(projectCont (lastX, COV ly))
            yield LineTo(projectCont (firstX, COV ly)) ]
          |> Array.ofList        
        Path(path, formatStyle defs (hideStroke style)) 

    | ProjectedLayered shapes, _ ->
        Combine(shapes |> Array.map (fun s -> drawShape defs style s))

    | ProjectedStack(_, shapes), _ ->
        Combine(shapes |> Array.map (fun s -> drawShape defs style s))

    | ProjectedInteractive(f, shape), _ ->
        drawShape defs style shape

// ------------------------------------------------------------------------------------------------
// Event handling
// ------------------------------------------------------------------------------------------------

module Events = 
  open Scales
  open Projections

  type MouseEventKind = Click | Move | Up | Down
  type TouchEventKind = Move | End | Start 

  type InteractiveEvent<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | MouseEvent of MouseEventKind * (Value<'vx> * Value<'vy>)    
    | TouchEvent of TouchEventKind * (Value<'vx> * Value<'vy>)    
    | MouseLeave

  let projectEvent scales projection event =
    match event with
    | MouseEvent(kind, (COV x, COV y)) -> MouseEvent(kind, projectInv scales (x, y) (invertProj projection))
    | TouchEvent(kind, (COV x, COV y)) -> TouchEvent(kind, projectInv scales (x, y) (invertProj projection))
    | MouseEvent _
    | TouchEvent _ -> failwith "TODO: projectEvent - not continuous"
    | MouseLeave -> MouseLeave

  let inScale s v = 
    match s, v with
    | Continuous(CO l, CO h), COV(CO v) -> v >= min l h && v <= max l h
    | Categorical(cats), CAR(v, _) -> cats |> Seq.exists ((=) v)
    | Continuous _, CAR _ -> failwith "inScale: Cannot test if categorical value is in continuous scale"
    | Categorical _, COV _ -> failwith "inScale: Cannot test if continuous value is in categorical scale"

  let inScales (sx, sy) event =
    match event with
    | MouseLeave -> true
    | MouseEvent(_, (x, y)) 
    | TouchEvent(_, (x, y)) -> inScale sx x && inScale sy y

  let rec triggerEvent<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ProjectedShape<'ux, 'uy>) (jse:Event) (event:InteractiveEvent<1,1>) = 
    let (Projected(projection, scales, shape)) = shape
    match shape with
    | ProjectedLine _
    | ProjectedText _
    | ProjectedColumn _
    | ProjectedBar _
    | ProjectedArea _ -> ()
    | ProjectedStyle(_, shape)
    | ProjectedOffset(_, shape) -> triggerEvent shape jse event
    | ProjectedStack(_, shapes)
    | ProjectedLayered shapes -> for shape in shapes do triggerEvent shape jse event
    | ProjectedInteractive(handlers, shape) ->
        let localEvent = projectEvent scales projection event
        if inScales scales localEvent then 
          for handler in handlers do 
            match localEvent, handler with
            | MouseEvent(MouseEventKind.Click, pt), EventHandler.Click(f) 
            | MouseEvent(MouseEventKind.Move, pt), MouseMove(f) 
            | MouseEvent(MouseEventKind.Up, pt), MouseUp(f) 
            | MouseEvent(MouseEventKind.Down, pt), MouseDown(f) -> 
                if jse <> null then jse.preventDefault()
                f (unbox jse) pt
            | TouchEvent(TouchEventKind.Move, pt), TouchMove(f) 
            | TouchEvent(TouchEventKind.Start, pt), TouchStart(f) 
            | TouchEvent(TouchEventKind.End, pt), TouchEnd(f) -> 
                if jse <> null then jse.preventDefault()
                f (unbox jse) pt
            | MouseLeave, EventHandler.MouseLeave f -> f (unbox jse) 
            | MouseLeave, _ 
            | TouchEvent(_, _), _  
            | MouseEvent(_, _), _  -> ()

        triggerEvent shape jse event

// ------------------------------------------------------------------------------------------------
// Integration
// ------------------------------------------------------------------------------------------------

module Compost = 
  open Scales
  open Projections
  open Drawing
  open Events
  open Svg
    
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

  let createSvg (width, height) viz = 
    let scaled = calculateScales viz
    let master = Scale((0.0, width), (height, 0.0))
    let projected = calculateProjections scaled master
    let defstyle = 
      { Fill = Solid(1.0, RGB(196, 196, 196))
        StrokeColor = (1.0, RGB(256, 0, 0))
        StrokeDashArray = []
        StrokeWidth = Pixels 2
        Animation = None 
        Cursor = "default"
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
      triggerEvent projected evt (MouseEvent(kind, (COV(CO x), COV(CO y))))

    let touchHandler kind el (evt:Event) =
      let evt = evt :?> TouchEvent
      let touch = evt.touches.[0]
      let x, y = getRelativeLocation el touch.pageX touch.pageY
      triggerEvent projected evt (TouchEvent(kind, (COV(CO x), COV(CO y))))

    h?div ["style"=>sprintf "width:%dpx;height:%dpx;margin:0px auto 0px auto" (int width) (int height)] [
      s?svg [
          "width"=>string (int width); "height"=> string(int height); 
          "click" =!> mouseHandler MouseEventKind.Click
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