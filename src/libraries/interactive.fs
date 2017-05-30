namespace TheGamma.Interactive

open Fable.Helpers
open Fable.Import.Browser

open TheGamma.Common
open TheGamma.Series
open TheGamma.Html
open TheGamma.Interactive.Compost

module InteractiveHelpers =
  let showAppAsync outputId size (data:series<_, _>) initial render update = async { 
    let id = "container" + System.Guid.NewGuid().ToString().Replace("-", "")
    h?div ["id" => id] [ ] |> renderTo (document.getElementById(outputId))        

    // Get data & wait until the element is created
    let! data = data.data |> Async.AwaitFuture 
    let mutable i = 10
    while i > 0 && document.getElementById(id) = null do
      do! Async.Sleep(10)
      i <- i - 1
    let element = document.getElementById(id)
    let size = 
      ( match size with Some w, _ -> w | _ -> element.clientWidth ),
      ( match size with _, Some h -> h | _ -> max 400. (element.clientWidth / 2.) ) 
    do
      try
        Compost.app outputId (initial data) (render data size) (update data)
      with e ->
        Log.exn("GUI", "Interactive rendering failed: %O", e) } 

  let showApp outputId size data initial render update = 
    showAppAsync outputId size data initial render update |> Async.StartImmediate 

  let calclateMax maxValue data = 
    let max = match maxValue with Some m -> m | _ -> Seq.max (Seq.map snd data)
    snd (Scales.adjustRange (0.0, max))

module CompostHelpers = 
  let (|Cont|) = function COV(CO x) -> x | _ -> failwith "Expected continuous value"
  let (|Cat|) = function CAR(CA x, r) -> x, r | _ -> failwith "Expected categorical value"
  let Cont x = COV(CO x)
  let Cat(x, r) = CAR(CA x, r)

open CompostHelpers

// ------------------------------------------------------------------------------------------------
// You Draw
// ------------------------------------------------------------------------------------------------

module Charts = 
  let renderBubbles (width, height) bc data = 
    let style selValue =
      let isDate = data |> Seq.exists (selValue >> isDate)
      if isDate then
        let values = data |> Array.map (selValue >> dateOrNumberAsNumber)
        let lo, hi = asDate(Seq.min values), asDate(Seq.max values)
        if (hi - lo).TotalDays <= 1. then
          fun _ (Cont v) -> formatTime(asDate(v))
        else
          fun _ (Cont v) -> formatDate(asDate(v))
      else Compost.defaultFormat

    let chart =       
      Layered [
        for x, y, s in data -> 
          let size = unbox (defaultArg s (box 2.))
          Bubble(COV(CO (dateOrNumberAsNumber x)), COV(CO (dateOrNumberAsNumber y)), size, size)
      ]

    let chart = 
      Style(
        (fun s -> 
          { s with 
              StrokeWidth = Pixels 0
              Fill = Solid(0.6, HTML bc)
              FormatAxisXLabel = style (fun (x, _, _) -> x)
              FormatAxisYLabel = style (fun (_, y, _) -> y) }),
        Axes(true, true, AutoScale(true, true, chart)) )

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg (width, height) chart
    ]

// ------------------------------------------------------------------------------------------------
// You Guess Line
// ------------------------------------------------------------------------------------------------

module YouDrawHelpers = 
  type YouDrawEvent = 
    | ShowResults
    | Draw of float * float

  type YouDrawState = 
    { Completed : bool
      Clip : float
      Data : (float * float)[]
      Guessed : (float * option<float>)[] 
      IsKeyDate : bool }

  let initState data clipx = 
    let isDate = data |> Seq.exists (fst >> isDate)
    let data = data |> Array.map (fun (k, v) -> dateOrNumberAsNumber k, v)
    { Completed = false
      Data = data
      Clip = clipx
      IsKeyDate = isDate
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

  let render (width, height) (markers:(float*obj)[]) (topLbl, leftLbl, rightLbl) (leftClr,rightClr,guessClr,markClr) (loy, hiy) trigger state = 
    let all = 
      [| for x, y in state.Data -> Cont x, Cont y |]
    let known = 
      [| for x, y in state.Data do if x <= state.Clip then yield Cont x, Cont y |]
    let right = 
      [| yield Array.last known
         for x, y in state.Data do if x > state.Clip then yield Cont x, Cont y |]
    let guessed = 
      [| yield Array.last known
         for x, y in state.Guessed do if y.IsSome then yield Cont x, Cont y.Value |]

    let lx, ly = (fst (Seq.head state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let rx, ry = (fst (Seq.last state.Data) + float state.Clip) / 2., loy + (hiy - loy) / 10.
    let tx, ty = float state.Clip, hiy - (hiy - loy) / 10.
    let setColor c s = { s with Font = "12pt sans-serif"; Fill=Solid(1.0, HTML c); StrokeColor=(0.0, RGB(0,0,0)) }
    let labels = 
      Shape.Layered [
        Style(setColor leftClr, Shape.Text(COV(CO lx), COV(CO ly), VerticalAlign.Baseline, HorizontalAlign.Center, leftLbl))
        Style(setColor rightClr, Shape.Text(COV(CO rx), COV(CO ry), VerticalAlign.Baseline, HorizontalAlign.Center, rightLbl))
        Style(setColor guessClr, Shape.Text(COV(CO tx), COV(CO ty), VerticalAlign.Baseline, HorizontalAlign.Center, topLbl))
      ]

    let LineStyle shape = 
      Style((fun s -> 
        { s with 
            Fill = Solid(1.0, HTML "transparent"); 
            StrokeWidth = Pixels 2; 
            StrokeDashArray = [Integer 5; Integer 5]
            StrokeColor=0.6, HTML markClr }), shape)
    let FontStyle shape = 
      Style((fun s -> { s with Font = "11pt sans-serif"; Fill = Solid(1.0, HTML markClr); StrokeColor = 0.0, HTML "transparent" }), shape)
    
    let loln, hiln = Scales.adjustRange (loy, hiy)
    let markers = [
        for i, (x, lbl) in Seq.mapi (fun i v -> i, v) markers do
          let kl, kt = if i % 2 = 0 then 0.90, 0.95 else 0.80, 0.85
          let ytx = loln + (hiln - loln) * kt
          let hiln = loln + (hiln - loln) * kl
          yield Line [(COV(CO x), COV(CO loln)); (COV(CO x), COV(CO hiln))] |> LineStyle
          yield Text(COV(CO x), COV(CO ytx), VerticalAlign.Middle, HorizontalAlign.Center, string lbl) |> FontStyle
      ]

    let coreChart = 
      Interactive(
        ( if state.Completed then []
          else
            [ MouseMove(fun evt (Cont x, Cont y) -> 
                if (int evt.buttons) &&& 1 = 1 then trigger(Draw(x, y)) )
              TouchMove(fun evt (Cont x, Cont y) -> 
                trigger(Draw(x, y)) )
              MouseDown(fun evt (Cont x, Cont y) -> trigger(Draw(x, y)) )
              TouchStart(fun evt (Cont x, Cont y) -> trigger(Draw(x, y)) ) ]),
        Shape.InnerScale
          ( None, Some(CO loy, CO hiy), 
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
                  Line guessed ) 
            ]) )

    let chart = 
      Style(
        (fun s -> 
          if state.IsKeyDate then 
            let lo, hi = asDate(fst state.Data.[0]), asDate(fst state.Data.[state.Data.Length-1])
            if (hi - lo).TotalDays <= 1. then
              { s with FormatAxisXLabel = fun _ (Cont v) -> formatTime(asDate(v)) }
            else
              { s with FormatAxisXLabel = fun _ (Cont v) -> formatDate(asDate(v)) }
          else s),
        Axes(true, true, 
          AutoScale(false, true, coreChart)))
    
    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg (width, height) chart
      h?div ["style"=>"padding-bottom:20px"] [
        h?button [
            yield "type" => "button"
            yield "click" =!> fun _ _ -> trigger ShowResults
            if state.Guessed |> Seq.last |> snd = None then
              yield "disabled" => "disabled"
          ] [ text "Show me how I did" ]
        ]
    ]
      
// ------------------------------------------------------------------------------------------------
// You Guess Bar & You Guess Column
// ------------------------------------------------------------------------------------------------

module YouGuessColsHelpers = 

  type YouGuessState = 
    { Completed : bool
      CompletionStep : float
      Default : float
      Maximum : float
      Data : (string * float)[]
      Guesses : Map<string, float> }

  type YouGuessEvent = 
    | ShowResults 
    | Animate 
    | Update of string * float

  let initState data maxValue =     
    { Completed = false
      CompletionStep = 0.0
      Data = data 
      Default = Array.averageBy snd data
      Maximum = InteractiveHelpers.calclateMax maxValue data
      Guesses = Map.empty }

  let update state evt = 
    match evt with
    | ShowResults -> { state with Completed = true }
    | Animate -> { state with CompletionStep = min 1.0 (state.CompletionStep + 0.05) }
    | Update(k, v) -> { state with Guesses = Map.add k v state.Guesses }

  let vega10 = ["#1f77b4"; "#ff7f0e"; "#2ca02c"; "#d62728"; "#9467bd"; "#8c564b"; "#e377c2"; "#7f7f7f"; "#bcbd22"; "#17becf" ]

  let renderCols (width, height) topLabel trigger state = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    let chart = 
      Axes(true, true, 
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
                        [ for clr, (lbl, value) in Seq.zip vega10 state.Data -> 
                            let sh = Style((fun s -> { s with Fill = Solid(0.2, HTML "#a0a0a0") }), Column(CA lbl, CO state.Maximum )) 
                            Shape.Padding((0., 10., 0., 10.), sh) ])
                    yield Stack
                      ( Horizontal, 
                        [ for clr, (lbl, value) in Seq.zip vega10 state.Data -> 
                            let alpha, value = 
                              match state.Completed, state.Guesses.TryFind lbl with
                              | true, Some guess -> 0.6, state.CompletionStep * value + (1.0 - state.CompletionStep) * guess
                              | _, Some v -> 0.6, v
                              | _, None -> 0.2, state.Default
                            let sh = Style((fun s -> { s with Fill = Solid(alpha, HTML clr) }), Column(CA lbl, CO value)) 
                            Shape.Padding((0., 10., 0., 10.), sh) ])
                    for clr, (lbl, value) in Seq.zip vega10 state.Data do
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
                          (fun s -> { s with Font = "13pt sans-serif"; Fill=Solid(1.0, HTML "#808080"); StrokeColor=(0.0, RGB(0,0,0)) }),
                          Text(x, y, VerticalAlign.Baseline, HorizontalAlign.Center, lbl) )
                  ]) ))))

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg (width, height) chart
      h?div ["style"=>"padding-bottom:20px"] [
        h?button [
            yield "type" => "button"
            yield "click" =!> fun _ _ -> trigger ShowResults
            if state.Guesses.Count <> state.Data.Length then
              yield "disabled" => "disabled"
          ] [ text "Show me how I did" ]
        ]
    ]


  let renderBars (width, height) topLabel trigger state = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    let chart = 
      Axes(true, false, 
        AutoScale(true, false, 
          Interactive
            ( ( if state.Completed then []
                else
                  [ EventHandler.MouseMove(fun evt (Cont x, Cat(y, _)) ->
                      if (int evt.buttons) &&& 1 = 1 then trigger (Update(y, x)) )
                    EventHandler.MouseDown(fun evt (Cont x, Cat(y, _)) ->
                      trigger (Update(y, x)) )
                    EventHandler.TouchStart(fun evt (Cont x, Cat(y, _)) ->
                      trigger (Update(y, x)) )
                    EventHandler.TouchMove(fun evt (Cont x, Cat(y, _)) ->
                      trigger (Update(y, x)) ) ] ),
              Style
                ( (fun s -> if state.Completed then s else { s with Cursor = "col-resize" }),
                  (Layered [
                    yield InnerScale(Some(CO 0., CO state.Maximum), None, 
                      Stack
                        ( Vertical, 
                          [ for clr, (lbl, value) in Seq.zip vega10 state.Data -> 
                              let sh = Style((fun s -> { s with Fill = Solid(0.2, HTML "#a0a0a0") }), Bar(CO state.Maximum, CA lbl)) 
                              Shape.Padding((10., 0., 10., 0.), sh) ]))
                    yield Stack
                      ( Vertical, 
                        [ for clr, (lbl, value) in Seq.zip vega10 state.Data -> 
                            let alpha, value = 
                              match state.Completed, state.Guesses.TryFind lbl with
                              | true, Some guess -> 0.6, state.CompletionStep * value + (1.0 - state.CompletionStep) * guess
                              | _, Some v -> 0.6, v
                              | _, None -> 0.2, state.Default
                            let sh = Style((fun s -> { s with Fill = Solid(alpha, HTML clr) }), Bar(CO value, CA lbl)) 
                            Shape.Padding((10., 0., 10., 0.), sh) ])

                    for clr, (lbl, _) in Seq.zip vega10 state.Data do 
                        let x = COV(CO (state.Maximum * 0.95))
                        let y = CAR(CA lbl, 0.5)
                        yield Style(
                          (fun s -> { s with Font = "13pt sans-serif"; Fill=Solid(1.0, HTML clr); StrokeColor=(0.0, RGB(0,0,0)) }),
                          Text(x, y, VerticalAlign.Middle, HorizontalAlign.End, lbl) )

                    for clr, (lbl, value) in Seq.zip vega10 state.Data do
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
                            Shape.Padding((10., 0., 10., 0.), line))
                    match topLabel with
                    | None -> ()
                    | Some lbl ->
                        let x = COV(CO (state.Maximum * 0.9))
                        let y = CAR(CA (fst state.Data.[state.Data.Length/2]), if state.Data.Length % 2 = 0 then 0.0 else 0.5)
                        yield Style(
                          (fun s -> { s with Font = "13pt sans-serif"; Fill=Solid(1.0, HTML "#808080"); StrokeColor=(0.0, RGB(0,0,0)) }),
                          Text(x, y, VerticalAlign.Baseline, HorizontalAlign.Center, lbl) )
                  ]) ))))

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg (width, height) chart
      h?div ["style"=>"padding-bottom:20px"] [
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
  let vega10 = ["#1f77b4"; "#ff7f0e"; "#2ca02c"; "#d62728"; "#9467bd"; "#8c564b"; "#e377c2"; "#7f7f7f"; "#bcbd22"; "#17becf" ]

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

  let initState maxValue data =     
    { Data = data 
      CompletionStep = 0.0
      Completed = false
      Colors = Seq.map2 (fun (lbl, _) clr -> lbl, clr) data vega10 |> dict 
      Assignments = Map.empty
      Selected = fst (Seq.head data)
      Maximum = InteractiveHelpers.calclateMax maxValue data }

  let update state evt = 
    match evt with
    | Animate -> { state with CompletionStep = min 1.0 (state.CompletionStep + 0.05) }
    | ShowResults -> { state with Completed = true }
    | SelectItem s -> { state with Selected = s }
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
  
  let renderBars (width, height) trigger (state:YouGuessState) = 
    if state.Completed && state.CompletionStep < 1.0 then
      window.setTimeout((fun () -> trigger Animate), 50) |> ignore
    let chart = 
      Axes(true, false, 
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
                                  console.log(actual, original)
                                  0.6, state.CompletionStep * actual + (1.0 - state.CompletionStep) * original, state.Colors.[assigned]
                              | _, Some assigned -> 0.6, original, state.Colors.[assigned]
                              | _, None -> 0.3, original, "#a0a0a0"

                            if i = state.Data.Length - 1 && state.Assignments.Count = 0 then
                              let txt = Text(COV(CO(state.Maximum * 0.05)), CAR(CA lbl, 0.5), Middle, Start, "Assign highlighted value to one of the bars by clicking on it!")
                              yield Style((fun s -> { s with Font = "13pt sans-serif"; Fill = Solid(1.0, HTML "#606060"); StrokeColor=(0.0, HTML "white") }), txt ) 

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
                    (fun s -> { s with Font = "11pt sans-serif"; Fill=Solid(al, HTML clr); StrokeColor=(0.0, RGB(0,0,0)) }),
                    Text(x, y, VerticalAlign.Middle, HorizontalAlign.Start, lbl) ) 
                  if not state.Completed then
                    yield 
                      Style((fun s -> { s with Cursor="pointer"; Fill=Solid(0.0, HTML "white")  }), Bar(CO 100., CA lbl))
                ])))
             
    let all = 
      Layered
        [ OuterScale(None, Some(Continuous(CO 0.0, CO 3.0)), labs) 
          OuterScale(None, Some(Continuous(CO 3.0, CO 10.0)), chart) ]

    h?div ["style"=>"text-align:center;padding-top:20px"] [
      Compost.createSvg (width, height) all
      h?div ["style"=>"padding-bottom:20px"] [
        h?button [
            yield "type" => "button"
            yield "click" =!> fun _ _ -> trigger ShowResults
            if state.Assignments.Count <> state.Data.Length then
              yield "disabled" => "disabled"
          ] [ text "Show me how I did" ]
        ]
    ]
    
// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

open TheGamma.Series
open TheGamma.Common

type YouGuessColsBarsKind = Cols | Bars

type YouGuessColsBars =
  { kind : YouGuessColsBarsKind
    data : series<string, float> 
    maxValue : float option
    topLabel : string option
    size : float option * float option }
  member y.setLabel(top) = { y with topLabel = Some top }
  member y.setMaximum(max) = { y with maxValue = Some max }
  member y.setSize(?width, ?height) = 
    let orElse (a:option<_>) b = if a.IsSome then a else b
    { y with size = (orElse width (fst y.size), orElse height (snd y.size)) }

  member y.show(outputId) =   
    InteractiveHelpers.showApp outputId y.size y.data
      (fun data -> YouGuessColsHelpers.initState data y.maxValue)
      (fun _ size -> 
          match y.kind with 
          | Bars -> YouGuessColsHelpers.renderBars size y.topLabel 
          | Cols -> YouGuessColsHelpers.renderCols size y.topLabel)
      (fun _ -> YouGuessColsHelpers.update)

type YouGuessLine = 
  { data : series<obj, float> 
    markers : series<float, obj> option
    clip : float option
    min : float option
    max : float option 
    markerColor : string option
    knownColor : string option
    unknownColor : string option 
    drawColor : string option 
    topLabel : string option
    knownLabel : string option
    guessLabel : string option 
    size : float option * float option }
  member y.setRange(min, max) = { y with min = Some min; max = Some max }
  member y.setClip(clip) = { y with clip = Some (dateOrNumberAsNumber clip) }
  member y.setColors(known, unknown) = { y with knownColor = Some known; unknownColor = Some unknown }
  member y.setDrawColor(draw) = { y with drawColor = Some draw }
  member y.setMarkerColor(marker) = { y with markerColor = Some marker }
  member y.setLabels(top, known, guess) = { y with knownLabel = Some known; topLabel = Some top; guessLabel = Some guess }
  member y.setSize(?width, ?height) = 
    let orElse (a:option<_>) b = if a.IsSome then a else b
    { y with size = (orElse width (fst y.size), orElse height (snd y.size)) }
  member y.setMarkers(markers) = { y with markers = Some markers }
  member y.show(outputId) = Async.StartImmediate <| async {
    let markers = defaultArg y.markers (series<string, float>.create(async.Return [||], "", "", ""))
    let! markers = markers.data |> Async.AwaitFuture
    let markers = markers |> Array.sortBy fst
    return! InteractiveHelpers.showAppAsync outputId y.size y.data
      (fun data ->
          let clipx = match y.clip with Some v -> v | _ -> dateOrNumberAsNumber (fst (data.[data.Length / 2]))
          YouDrawHelpers.initState (Array.sortBy (fst >> dateOrNumberAsNumber) data) clipx)
      (fun data size ->           
          let loy = match y.min with Some v -> v | _ -> data |> Seq.map snd |> Seq.min
          let hiy = match y.max with Some v -> v | _ -> data |> Seq.map snd |> Seq.max       
          let lc, dc, gc, mc = 
            defaultArg y.knownColor "#606060", defaultArg y.unknownColor "#FFC700", 
            defaultArg y.drawColor "#808080", defaultArg y.markerColor "#C65E31"          
          YouDrawHelpers.render size markers
            (defaultArg y.topLabel "", defaultArg y.knownLabel "", defaultArg y.guessLabel "") 
            (lc,dc,gc,mc) (loy, hiy)) 
      (fun _ -> YouDrawHelpers.handler) } 

type YouGuessSortBars = 
  { data : series<string, float> 
    maxValue : float option 
    size : float option * float option }
  member y.setMaximum(max) = { y with maxValue = Some max }
  member y.setSize(?width, ?height) = 
    let orElse (a:option<_>) b = if a.IsSome then a else b
    { y with size = (orElse width (fst y.size), orElse height (snd y.size)) }
  member y.show(outputId) = 
    InteractiveHelpers.showApp outputId y.size y.data
      (YouGuessSortHelpers.initState y.maxValue)
      (fun _ size -> YouGuessSortHelpers.renderBars size)
      (fun _ -> YouGuessSortHelpers.update)

type CompostBubblesChartSet =
  { data : series<obj, obj> 
    selectY : obj -> obj
    selectX : obj -> obj
    selectSize : option<obj -> obj>
    bubbleColor : string option
    size : float option * float option }
  member y.setColors(?bubbleColor) = 
    { y with bubbleColor = defaultArg bubbleColor y.bubbleColor }
  member y.setSize(?width, ?height) = 
    let orElse (a:option<_>) b = if a.IsSome then a else b
    { y with size = (orElse width (fst y.size), orElse height (snd y.size)) }
  member y.show(outputId) = 
    InteractiveHelpers.showApp outputId y.size y.data
      (fun _ -> ())
      (fun data size _ _ -> 
        let ss = match y.selectSize with Some f -> (fun x -> Some(f x)) | _ -> (fun _ -> None)
        let data = data |> Array.map (fun (_, v) -> y.selectX v, y.selectY v, ss v)
        let bc = defaultArg y.bubbleColor "#20a030"
        Charts.renderBubbles size bc data)
      (fun _ _ _ -> ())

type CompostBubblesChart<'k, 'v>(data:series<'k, 'v>) = 
  member c.set(x:'v -> obj, y:'v -> obj, ?size:'v -> obj) = 
    { CompostBubblesChartSet.data = unbox data
      selectX = unbox x; selectY = unbox y
      selectSize = unbox size; size = None, None
      bubbleColor = None }

type CompostCharts() = 
  member c.bubbles(data:series<'k, 'v>) = CompostBubblesChart<'k, 'v>(data)

type compost = 
  static member charts = CompostCharts()

type youguess = 
  static member columns(data:series<string, float>) = 
    { YouGuessColsBars.data = data; topLabel = None; kind = Cols; maxValue = None; size = None, None }
  static member bars(data:series<string, float>) = 
    { YouGuessColsBars.data = data; topLabel = None; kind = Bars; maxValue = None; size = None, None }
  static member sortBars(data:series<string, float>) = 
    { YouGuessSortBars.data = data; maxValue = None; size = None, None }
  static member line(data:series<obj, float>) =
    { YouGuessLine.data = data; clip = None; min = None; max = None 
      markerColor = None; guessLabel = None; topLabel = None; knownLabel = None; markers = None
      knownColor = None; unknownColor = None; drawColor = None; size = None, None }
