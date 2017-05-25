#if INTERACTIVE
#r "../../src/libraries/bin/Debug/Fable.Core.dll"
#r "../../src/libraries/bin/Debug/libraries.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Compost
#endif
open TheGamma
open TheGamma.Html
open TheGamma.Interactive.Compost
open NUnit.Framework

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)


let rec renderChildren childs = 
  Seq.map render childs |> String.concat ""

and renderAttrs attrs = 
  let s = Seq.map renderAttr attrs |> String.concat ""
  if s <> "" then " " + s else s

and renderAttr = function
  | k, Event _ -> ""
  | k, Attribute v -> sprintf "%s='%s'" k v
  | k, Property _ -> failwith "Properties not supported"

and render = function
  | DomNode.Text s -> s
  | Element(null, tag, attrs, childs, None) ->
      sprintf "<%s%s>%s</%s>" tag (renderAttrs attrs) (renderChildren childs) tag
  | Element(ns, tag, attrs, childs, None) ->
      sprintf "<%s xmlns='%s'%s>%s</%s>" tag ns (renderAttrs attrs) (renderChildren childs) tag
  | Element(_, _, _, _, Some _) -> failwith "On render not supported"
  | Part _ | Delayed _ -> failwith "Delayed and parts not supported"

#if INTERACTIVE
let tmp = System.IO.Path.GetTempFileName() + ".html"
let preview html = 
  System.IO.File.WriteAllText(tmp, html)
  System.Diagnostics.Process.Start(tmp) |> ignore
#else
let preview html = ()
#endif

// --------------------------------------------------------------------------------------
// TESTS: Call chains and nesting
// --------------------------------------------------------------------------------------

[<Test>]
let zz() = 
  let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
  let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
  let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
  let proj = Projections.Scale((0.0, 100.0), (0.0, 100.0))
  let px, py = Projections.project sx sy pt proj
  let npt = Projections.projectInv (sx, sy) (px, py) proj
  // npt = pt
  ()

[<Test>]
let zzzz() = 
  let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
  let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
  let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
  let proj = Projections.Rescale((0.5, 0.6), (0.0, 1.0), Projections.Scale((0.0, 100.0), (0.0, 100.0)))
  let px, py = Projections.project sx sy pt proj
  let npt = Projections.projectInv (sx, sy) (px, py) proj
  // npt = pt
  ()

[<Test>]
let zzzzz() = 
  let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
  let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
  let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
  let proj = Projections.Rescale((0.0, 1.0), (0.5, 0.6), Projections.Scale((0.0, 100.0), (0.0, 100.0)))
  let px, py = Projections.project sx sy pt proj
  let npt = Projections.projectInv (sx, sy) (px, py) proj
  // npt = pt
  ()

[<Test>]
let zzzzzzz() = 
  let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
  let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
  let (xl, xh), (yl, yh) = Projections.getExtremes sx, Projections.getExtremes sy
  let proj = 
    Projections.Rescale((0.0, 1.0), (0.5, 0.6), 
      Projections.Padding((10.,10.,10.,10.), 
        (xl, xh, yl, yh),
        Projections.Scale((0.0, 100.0), (0.0, 100.0))))

  let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
  let px, py = Projections.project sx sy pt proj
  let npt = Projections.projectInv (sx, sy) (px, py) proj
  // npt = pt
  ()

[<Test>]
let zzzzzzzz() = 
  let sx : Scale<1> = Continuous(CO 1000.0, CO 2000.0)
  let sy : Scale<1> = Categorical [| CA "A"; CA "B"; CA "C" |]
  let (xl, xh), (yl, yh) = Projections.getExtremes sx, Projections.getExtremes sy
  let proj = 
    Projections.Padding((10.,10.,5.,5.), 
      (xl, xh, yl, yh),
      Projections.Rescale((0.0, 1.0), (0.5, 0.6), 
        Projections.Scale((0.0, 500.0), (0.0, 500.0))))

  let pt = (COV (CO 1200.), CAR (CA "C", 0.5))
  let px, py = Projections.project sx sy pt proj
  let npt = Projections.projectInv (sx, sy) (px, py) proj
  // npt = pt
  ()

[<Test>]
let zzzzzz() = 
  let s1 : Shape<1,1> = Stack(Vertical, [ Bar(CO 50.0, CA "A"); Bar(CO 100.0, CA "B") ])
  let s2 : Shape<1,1> = Stack(Vertical, [ Bar(CO 50.0, CA "A"); Bar(CO 100.0, CA "B") ])
  let s1e = Interactive([ Click(fun _ a -> printfn "1: %A" a) ], s1)
  let s2e = Interactive([ Click(fun _ a -> printfn "2: %A" a) ], s2)
  let s = 
    Layered
      [ OuterScale(None, Some(Continuous(CO 0.0, CO 1.0)), s1e) 
        OuterScale(None, Some(Continuous(CO 1.0, CO 2.0)), s2e) ]

  let scaled = Scales.calculateScales s
  let master = Projections.Scale((0.0, 500.0), (500.0, 0.0))
  let projected = Projections.calculateProjections scaled master
  
  Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 100.))))
  Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 150.))))
  Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 350.))))
  Events.triggerEvent projected null (Events.MouseEvent(Events.Click, (COV (CO 250.), COV (CO 400.))))

[<Test>]
let zzz() = 
  let s1 : Shape<1,1> = Stack(Vertical, [ Bar(CO 50.0, CA "A"); Bar(CO 100.0, CA "B") ])
  let s2 : Shape<1,1> = Line [ COV(CO 0.0), COV(CO 0.0); COV(CO 50.0), COV(CO 10.0); COV(CO 100.0), COV(CO 0.0) ]
  let s3 : Shape<1,1> = Bar(CO 50.0, CA "C")
  let s = 
    Layered
      [ OuterScale(None, Some(Continuous(CO 0.0, CO 2.0)), Axes(true, true, s1)) 
        OuterScale(None, Some(Continuous(CO 2.0, CO 3.0)), Axes(true, true, s2))
        OuterScale(None, Some(Continuous(CO 3.0, CO 4.0)), Padding((0.,0.,0.,100.), s3)) ]
  let svg = Compost.createSvg (500.0, 500.0) s
  preview (render svg)

  render svg