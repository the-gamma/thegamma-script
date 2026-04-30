module TheGamma.Tests.Compost

open Fable.Mocha
open TheGamma
open TheGamma.Html
open TheGamma.Interactive.Compost
open TheGamma.Interactive.Compost.Derived

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

// --------------------------------------------------------------------------------------
// Tests
// --------------------------------------------------------------------------------------

let tests = testList "Compost" [
  testCase "Scale and project continuous/categorical point" <| fun () ->
    let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
    let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
    let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
    let proj = Projections.Scale((0.0, 100.0), (0.0, 100.0))
    let px, py = Projections.project sx sy pt proj
    let npt = Projections.projectInv (sx, sy) (px, py) proj
    ()

  testCase "Rescale projection with continuous/categorical point" <| fun () ->
    let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
    let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
    let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
    let proj = Projections.Rescale((0.5, 0.6), (0.0, 1.0), Projections.Scale((0.0, 100.0), (0.0, 100.0)))
    let px, py = Projections.project sx sy pt proj
    let npt = Projections.projectInv (sx, sy) (px, py) proj
    ()

  testCase "Rescale in y dimension" <| fun () ->
    let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
    let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
    let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
    let proj = Projections.Rescale((0.0, 1.0), (0.5, 0.6), Projections.Scale((0.0, 100.0), (0.0, 100.0)))
    let px, py = Projections.project sx sy pt proj
    let npt = Projections.projectInv (sx, sy) (px, py) proj
    ()

  testCase "Rescale with padding projection" <| fun () ->
    let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
    let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
    let (xl, xh), (yl, yh) = Scales.getExtremes sx, Scales.getExtremes sy
    let proj =
      Projections.Rescale((0.0, 1.0), (0.5, 0.6),
        Projections.Padding((10.,10.,10.,10.),
          (xl, xh, yl, yh),
          Projections.Scale((0.0, 100.0), (0.0, 100.0))))
    let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
    let px, py = Projections.project sx sy pt proj
    let npt = Projections.projectInv (sx, sy) (px, py) proj
    ()

  testCase "Padding with rescale projection" <| fun () ->
    let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
    let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
    let (xl, xh), (yl, yh) = Scales.getExtremes sx, Scales.getExtremes sy
    let proj =
      Projections.Padding((10.,10.,5.,5.),
        (xl, xh, yl, yh),
        Projections.Rescale((0.0, 1.0), (0.5, 0.6),
          Projections.Scale((0.0, 500.0), (0.0, 500.0))))
    let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
    let px, py = Projections.project sx sy pt proj
    let npt = Projections.projectInv (sx, sy) (px, py) proj
    ()

  testCase "Layered interactive shapes trigger events correctly" <| fun () ->
    let s1 : Shape<1,1> = Stack(Vertical, [ Bar(CO 50.0, CA "A"); Bar(CO 100.0, CA "B") ])
    let s2 : Shape<1,1> = Stack(Vertical, [ Bar(CO 50.0, CA "A"); Bar(CO 100.0, CA "B") ])
    let s1e = Interactive([ Click(fun _ a -> printfn "1: %A" a) ], s1)
    let s2e = Interactive([ Click(fun _ a -> printfn "2: %A" a) ], s2)
    let s =
      Layered
        [ OuterScale(None, Some(Continuous(CO 0.0, CO 1.0)), s1e)
          OuterScale(None, Some(Continuous(CO 1.0, CO 2.0)), s2e) ]
    let scaled = Scales.calculateScales Compost.defstyle s
    let master = Projections.Scale((0.0, 500.0), (500.0, 0.0))
    let projected = Projections.calculateProjections scaled master
    Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 100.))))
    Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 150.))))
    Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 350.))))
    Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 400.))))
]
