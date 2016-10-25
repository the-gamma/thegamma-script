module TheGamma.Html

open Fable.Core
open Fable.Helpers
open Fable.Import.Browser

module FsOption = FSharp.Core.Option

[<Fable.Core.Emit("jQuery($0).chosen()")>]
let private chosen (el:HTMLElement) : unit = failwith "JS"

[<Fable.Core.Emit("jQuery($0).on($1, $2)")>]
let private on (el:HTMLElement) (evt:string) (f:unit -> unit) : unit = failwith "JS"

[<Emit("$0[$1]")>]
let private getProperty (o:obj) (s:string) = failwith "!"

[<Fable.Core.Emit("event")>]
let private event () : Event = failwith "JS"

type DomAttribute = 
  | Event of (HTMLElement -> Event -> unit)
  | Attribute of string

type DomNode = 
  | Text of string
  | Delayed of (string -> unit)
  | Element of tag:string * attributes:(string * DomAttribute)[] * children : DomNode[] * onRender : (HTMLElement -> unit) option
  | Part of func:(HTMLElement -> unit)

let createTree tag args children =
    let attrs = ResizeArray<_>()
    let props = ResizeArray<_>()
    for k, v in args do
      match k, v with 
      | k, Attribute v ->
          attrs.Add (k, box v)
      | k, Event f ->
          props.Add ("on" + k, box (fun o -> f (getProperty o "target") (event()) ))
    let attrs = JsInterop.createObj attrs
    let props = JsInterop.createObj (Seq.append ["attributes", attrs] props)
    let elem = Virtualdom.h(tag, props, children)
    elem

let mutable counter = 0

let rec renderVirtual node = 
  match node with
  | Text(s) -> 
      box s
  | Element(tag, attrs, children, None) ->
      createTree tag attrs (Array.map renderVirtual children)
  | Delayed(func) ->
      counter <- counter + 1
      createTree "div" [] [| box "delayed..." |]
  | Element _ ->
      failwith "renderVirtual: Does not support elements with after-render handlers"
  | Part _ ->
      failwith "renderVirtual: Does not support parts"

let rec render node = 
  match node with
  | Text(s) -> 
      document.createTextNode(s) :> Node, ignore

  | Delayed(func) ->
      counter <- counter + 1
      let el = document.createElement("div")
      el.id <- sprintf "delayed_%d" counter
      el :> Node, (fun () -> func el.id)

  | Part(func) ->
      let el = document.createElement("div")
      el :> Node, (fun () -> func el)

  | Element(tag, attrs, children, f) ->
      let el = document.createElement(tag)
      let rc = Array.map render children
      for c, _ in rc do el.appendChild(c) |> ignore
      for k, a in attrs do 
        match a with
        | Attribute(v) -> el.setAttribute(k, v)
        | Event(f) -> el.addEventListener(k, U2.Case1(EventListener(f el)))
      let onRender () = 
        for _, f in rc do f()
        f |> FsOption.iter (fun f -> f el)
      el :> Node, onRender

let renderTo (node:HTMLElement) dom = 
  while box node.lastChild <> null do ignore(node.removeChild(node.lastChild))
  let el, f = render dom
  node.appendChild(el) |> ignore
  f()
  
let text s = Text(s)
let (=>) k v = k, Attribute(v)
let (=!>) k f = k, Event(f)


type El() = 
  static member (?) (_:El, n:string) = fun a b ->
    let n, f = 
      if n <> "chosen" then n, None
      else "select", Some (fun el ->
        chosen el
        for k, v in a do
          match v with
          | Event f -> on el k (fun () -> f el (event()))
          | _ -> ()
      )
    Element(n, Array.ofList a, Array.ofList b, f)

  member x.delayed(f) =
    Delayed(f)

  member x.part (initial:'State) (fold:'State -> 'Event -> 'State) = 
    let evt = Control.Event<_>()
    let mutable state = initial
    let mutable container = None
    let mutable renderer = None
    let render () =
      match container, renderer with
      | Some el, Some r -> r state |> renderTo el
      | _ -> ()
    evt.Publish.Add(fun e -> state <- fold state e; render ())

    evt.Trigger,
    fun (r:'State -> DomNode) ->
      renderer <- Some r
      Part(fun el -> 
        container <- Some el
        render() )

let h = El()
