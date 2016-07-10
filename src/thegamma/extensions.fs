module Fable.Extensions
open Fable.Core
open Fable.Import.JS
open Fable.Import.Browser

[<Emit("JSON.stringify($0)")>]
let jsonStringify json : string = failwith "JS Only"

[<Emit("JSON.parse($0)")>]
let jsonParse<'R> (str:string) : 'R = failwith "JS Only"

module Http =
  /// Send HTTP request asynchronously
  /// (does not handle errors properly)
  let Request(meth, url, data) =
    Async.FromContinuations(fun (cont, _, _) ->
      let xhr = XMLHttpRequest.Create()
      xhr.``open``(meth, url, true)
      xhr.onreadystatechange <- fun _ ->
        if xhr.readyState > 3. && xhr.status = 200. then
          cont(xhr.responseText)
        obj()
      xhr.send(defaultArg data "") )

type Future<'T> = 
  abstract Then : ('T -> unit) -> unit

type Microsoft.FSharp.Control.Async with
  static member AsFuture(op, ?start) = 
    let mutable res = None
    let mutable handlers = []
    let mutable running = false

    let ensureStarted() = 
      if not running then 
        running <- true
        async { let! r = op
                res <- Some r
                for h in handlers do h r  } |> Async.StartImmediate
    if start = Some true then ensureStarted()

    { new Future<_> with
        member x.Then(f) = 
          ensureStarted()
          match res with
          | Some v -> f v
          | None -> handlers <- f::handlers }


