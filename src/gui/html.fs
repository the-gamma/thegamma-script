module TheGamma.Html
module FsOption = FSharp.Core.Option
open Fable.Import.Browser
open Fable.Core

[<Fable.Core.Emit("jQuery($0).chosen()")>]
let private chosen (el:HTMLElement) : unit = failwith "JS"

[<Fable.Core.Emit("jQuery($0).on($1, $2)")>]
let private on (el:HTMLElement) (evt:string) (f:unit -> unit) : unit = failwith "JS"

[<Fable.Core.Emit("event")>]
let private event () : Event = failwith "JS"

type DomAttribute = 
  | Event of (HTMLElement -> Event -> unit)
  | Property of string

type DomNode = 
  | Text of string
  | Element of tag:string * attributes:(string * DomAttribute)[] * children : DomNode[] * onRender : (HTMLElement -> unit) option
  | Part of initial:obj * func:(HTMLElement -> obj -> (obj -> unit) -> unit)

let rec render node = 
  match node with
  | Text(s) -> 
      document.createTextNode(s) :> Node, ignore

  | Part(initial, func) ->
      let el = document.createElement("div")
      let rec update state =
        func el state update
      el :> Node, (fun () -> update initial)

  | Element(tag, attrs, children, f) ->
      let el = document.createElement(tag)
      let rc = Array.map render children
      for c, _ in rc do el.appendChild(c) |> ignore
      for k, a in attrs do 
        match a with
        | Property(v) -> el.setAttribute(k, v)
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
let (=>) k v = k, Property(v)
let (=!>) k f = k, Event(f)


type El() = 
  static member (?) (_:El, n:string) = fun a b ->
    let f = 
      if n <> "select" then None
      else Some (fun el ->
        chosen el
        for k, v in a do
          match v with
          | Event f -> on el k (fun () -> f el (event()))
          | _ -> ()
      )
    Element(n, Array.ofList a, Array.ofList b, f)

  member x.part (initial:'State) (func:HTMLElement -> 'State -> ('State -> unit) -> unit) =
    Part(unbox initial, unbox func)

let h = El()





