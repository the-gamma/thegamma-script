namespace TheGamma.Interactive 

open TheGamma.Html
open Fable.Import.Browser
open Fable.Helpers

module Visualizations = 

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

  // ----------------------------------------------------------------------------------------------
  // Domain that users see
  // ----------------------------------------------------------------------------------------------
  
  type continuous<[<Measure>] 'u> = CO of float<'u> 

  type EventHandler<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | MouseMove of ((continuous<'vx> * continuous<'vy>) -> unit)
    | MouseUp of ((continuous<'vx> * continuous<'vy>) -> unit)
    | MouseDown of ((continuous<'vx> * continuous<'vy>) -> unit)

  type Shape<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | Line of seq<continuous<'vx> * continuous<'vy>>
    | Layered of seq<Shape<'vx, 'vy>>
    | Axes of Shape<'vx, 'vy>
    | Interactive of seq<EventHandler<'vx, 'vy>> * Shape<'vx, 'vy>

  // ----------------------------------------------------------------------------------------------
  // Calculating scales
  // ----------------------------------------------------------------------------------------------

  type Scale<[<Measure>] 'v> =
    | Continuous of continuous<'v> * continuous<'v>

  type ScaledShapeInner<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | ScaledLine of (continuous<'vx> * continuous<'vy>)[]
    | ScaledLayered of ScaledShape<'vx, 'vy>[]
    | ScaledAxes of   
        (continuous<'vx>[] * continuous<'vy>[]) * 
        ((continuous<'vx>*string)[] * (continuous<'vy>*string)[]) * 
        ScaledShape<'vx, 'vy>
    | ScaledInteractive of seq<EventHandler<'vx, 'vy>> * ScaledShape<'vx, 'vy>

  and ScaledShape<[<Measure>] 'vx, [<Measure>] 'vy> =
    Scaled of (Scale<'vx> * Scale<'vy>) * ScaledShapeInner<'vx, 'vy>


  [<Fable.Core.Emit("$0.toFixed($1)")>]
  let toFixed (num:float) (decs:float) : string = failwith "JS"

  let generateRange (CO (lo:float<'u>)) (CO (hi:float<'u>)) = 
    let lo, hi = unbox lo, unbox hi 
    let mag = 10. ** (floor (max (log10 lo) (log10 hi)) - 1.0)
    let decimals = max 0. (-(log10 mag))
    let alo, ahi = floor (lo / mag) * mag, ceil (hi / mag) * mag
    let range = [| for v in alo .. mag .. ahi -> CO (unbox<float<'u>> v) |]
    let tmag = mag * (float (range.Length / 5)) // generate ~5 text labels
    [| for v in alo .. tmag .. ahi -> CO (unbox<float<'u>> v), toFixed v decimals |], 
    range

  let unionScales s1 s2 =
    match s1, s2 with
    | Continuous(l1, h1), Continuous(l2, h2) -> Continuous(min l1 l2, max h1 h2)

  // From the leafs to the root, calculate the scales of
  // everything (composing sales of leafs to get scale of root)

  let rec calculateScales<[<Measure>] 'ux, [<Measure>] 'uy> (shape:Shape<'ux, 'uy>) = 
    match shape with
    | Line line -> 
        let line = line |> Seq.toArray
        let xs = line |> Array.map fst 
        let ys = line |> Array.map snd
        let x0, x1 = Array.min xs, Array.max xs
        let y0, y1 = Array.min ys, Array.max ys
        let sx, sy = Continuous(x0, x1), Continuous(y0, y1)
        Scaled((sx, sy), ScaledLine(line))

    | Axes shape ->
        let scaled = calculateScales shape        
        let (Scaled((Continuous(lx, hx), Continuous(ly, hy)), _)) = scaled
        let (rxt,rx), (ryt,ry) = generateRange lx hx, generateRange ly hy
        let scales = Continuous(Array.head rx, Array.last rx), Continuous(Array.head ry, Array.last ry)
        Scaled(scales, ScaledAxes((rx, ry), (rxt, ryt), scaled))

    | Layered shapes ->
        let shapes = shapes |> Array.ofSeq
        let scaled = shapes |> Array.map calculateScales 
        let sxs = scaled |> Array.map (fun (Scaled((sx, _), _)) -> sx)
        let sys = scaled |> Array.map (fun (Scaled((_, sy), _)) -> sy)
        Scaled((Array.reduce unionScales sxs, Array.reduce unionScales sys), ScaledLayered scaled)

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
    | ProjectedLine of (continuous<'vx> * continuous<'vy>)[]
    | ProjectedLayered of ProjectedShape<'vx, 'vy>[]
    | ProjectedAxes of   
        (continuous<'vx>[] * continuous<'vy>[]) * 
        ((continuous<'vx>*string)[] * (continuous<'vy>*string)[]) * 
        ProjectedShape<'vx, 'vy>
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
    | Scaled(scales, ScaledLine line) -> 
        Projected(projection, scales, ProjectedLine line)

    | Scaled(scales, ScaledAxes(r, rg, shape)) ->
        let ppad = Padding(10.0, 10.0, 40.0, 40.0, projection)
        Projected(ppad, scales, ProjectedAxes(r, rg, calculateProjections shape ppad))
        
    | Scaled(scales, ScaledLayered shapes) ->
        Projected(projection, scales, ProjectedLayered(shapes |> Array.map (fun s -> calculateProjections s projection)))

    | Scaled(scales, ScaledInteractive(f, shape)) ->
        Projected(projection, scales, ProjectedInteractive(f, calculateProjections shape projection))

  // ----------------------------------------------------------------------------------------------
  // Drawing
  // ----------------------------------------------------------------------------------------------

  let rec drawShape<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ProjectedShape<'ux, 'uy>) = 
    let (Projected(projection, (sx, sy), shape)) = shape
    let projectCont (x, y) = 
      match project sx sy (x, y) projection with
      | (CO x), (CO y) -> x, y

    match shape with
    | ProjectedLine line -> 
        let path = 
          [ yield MoveTo(projectCont (Seq.head line)) 
            for pt in Seq.skip 1 line do yield LineTo (projectCont pt) ]
          |> Array.ofList
        Path(path, "fill:transparent; stroke:rgb(0,164,0); stroke-width:1") 

    | ProjectedAxes((rx, ry), (rxt, ryt), shape) ->
        let (Continuous(lx, hx), Continuous(ly, hy)) = sx, sy
        let offs (dx, dy) (x, y) = (x+dx, y+dy)
        Combine   
           [| yield Path(
                [|yield MoveTo (projectCont (lx, hy)) 
                  yield LineTo (projectCont (lx, ly))
                  yield LineTo (projectCont (hx, ly)) |], "fill:transparent; stroke:rgb(0,0,0); stroke-width:2");
              for x, xl in rxt do 
                yield Text(offs (0., 10.0) (projectCont (x, ly)), xl, "alignment-baseline:hanging;text-anchor:middle;font:9pt sans-serif")
              for y, yl in ryt do 
                yield Text(offs (-10., 0.0) (projectCont (lx, y)), yl, "alignment-baseline:middle;text-anchor:end;font:9pt sans-serif")
              yield Path(
                [|for x in rx do
                    yield MoveTo (projectCont (x, ly))
                    yield LineTo (projectCont (x, hy))
                  for y in ry do
                    yield MoveTo (projectCont (lx, y))
                    yield LineTo (projectCont (hx, y)) |], "fill:transparent; stroke:rgb(196,196,196); stroke-width:1") 
              yield drawShape shape |]     

    | ProjectedLayered shapes ->
        Combine(shapes |> Array.map (fun s -> drawShape s))

    | ProjectedInteractive(f, shape) ->
        drawShape shape

  // ----------------------------------------------------------------------------------------------
  // Event handling
  // ----------------------------------------------------------------------------------------------

  type MouseEventKind = Move | Up | Down
  type InteractiveEvent<[<Measure>] 'vx, [<Measure>] 'vy> = 
    | MouseEvent of MouseEventKind * (continuous<'vx> * continuous<'vy>)    

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

  let inScales (Continuous(CO x1, CO x2), Continuous(CO y1, CO y2)) event =
    match event with
    | MouseEvent(_, (CO x, CO y)) -> 
        x > min x1 x2 && x < max x1 x2 &&
          y > min y1 y2 && y < max y1 y2 

  let rec triggerEvent<[<Measure>] 'ux, [<Measure>] 'uy> (shape:ProjectedShape<'ux, 'uy>) (event:InteractiveEvent<1,1>) = 
    let (Projected(projection, scales, shape)) = shape
    match shape with
    | ProjectedLine _ -> ()
    | ProjectedAxes(_, _, shape) -> triggerEvent shape event
    | ProjectedLayered shapes -> for shape in shapes do triggerEvent shape event
    | ProjectedInteractive(handlers, shape) ->
        let localEvent = projectEvent scales projection event
        if inScales scales localEvent then 
          for handler in handlers do 
            match localEvent, handler with
            | MouseEvent(MouseEventKind.Move, pt), MouseMove(f) -> f pt
            | MouseEvent(MouseEventKind.Up, pt), MouseUp(f) -> f pt
            | MouseEvent(MouseEventKind.Down, pt), MouseDown(f) -> f pt
            | MouseEvent(_, _), _  -> ()

        triggerEvent shape event

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
    | StartDrawing
    | EndDrawing
    | Draw of float * float

  type YouDrawState = 
    { Drawing : bool
      Data : (continuous<1> * continuous<1>)[] }

  let state = 
    { Drawing = false
      Data = [| for x in 0. .. 0.1 .. 6.28 -> CO x, CO x |] }

  let handler state evt = 
    state

  let render trigger state = 
    let chart = 
      Axes
        (Interactive(
          [ 
            MouseMove(fun (CO x, CO y) -> printfn "Move: (%A,%A)" x y)
            MouseDown(fun (CO x, CO y) -> printfn "Down: (%A,%A)" x y) ],
          (Layered [|
            Line [| for x in 0. .. 0.1 .. 6.28 -> CO x, CO (cos x) |]
            Line state.Data
          |])
        ))

    let scaled = calculateScales chart
    let master = Scale((0.0, 800.0), (500.0, 0.0))
    let projected = calculateProjections scaled master
    console.log(projected)
    let svg = drawShape projected

    let mouseHandler kind el (evt:Event) =
      let evt = evt :?> MouseEvent
      let rec getOffset (parent:HTMLElement) (x, y) = 
        if parent = null then (x, y)
        else getOffset (unbox parent.offsetParent) (x-parent.offsetLeft, y-parent.offsetTop)
      let rec getParent (parent:HTMLElement) = 
        if parent.offsetParent <> null then parent 
        else getParent parent.parentElement

      let x, y = getOffset (getParent el) (evt.pageX, evt.pageY)
      triggerEvent projected (MouseEvent(kind, (CO x, CO y)))

    h?div ["style"=>"width:800px;height:500px"] [
      s?svg [
          "width"=>"800"; "height"=>"500"; 
          "mousemove" =!> mouseHandler MouseEventKind.Move
          "mousedown" =!> mouseHandler MouseEventKind.Down
          "mouseup" =!> mouseHandler MouseEventKind.Up
        ] (List.ofSeq (renderSvg svg))

    ]

  let main id = app id state render handler

type interactive = 
  { a:int }
  static member create() = { a = 0 }
  member y.show(outputId) =   
    async { 
      InteractiveHelpers.main outputId
    } |> Async.StartImmediate  
