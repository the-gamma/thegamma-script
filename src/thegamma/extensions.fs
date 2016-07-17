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
  static member AwaitFuture (f:Future<'T>) = Async.FromContinuations(fun (cont, _, _) ->
    f.Then(cont))

  static member AsFuture(op, ?start) = 
    let mutable res = Choice1Of3()
    let mutable handlers = []
    let mutable running = false

    let trigger h = 
      match res with
      | Choice1Of3 () -> handlers <- h::handlers 
      | Choice2Of3 v -> h v
      | Choice3Of3 e -> raise e

    let ensureStarted() = 
      if not running then 
        running <- true
        async { try 
                  let! r = op
                  res <- Choice2Of3 r                  
                with e ->
                  res <- Choice3Of3 e
                for h in handlers do trigger h } |> Async.StartImmediate
    if start = Some true then ensureStarted()

    { new Future<_> with
        member x.Then(f) = 
          ensureStarted()
          trigger f }

  static member StartAsFuture(op) = Async.AsFuture(op, true)

module Async = 
  let rec map f l = async {
    match l with 
    | x::xs -> 
        let! y = f x
        let! ys = map f xs
        return y::ys
    | [] -> return [] }

  let rec foldMap f st l = async {
    match l with
    | x::xs ->
        let! st, y = f st x
        let! st, ys = foldMap f st xs
        return st, y::ys
    | [] -> return st, [] }

  let rec fold f st l = async {
    match l with
    | x::xs ->
        let! st = f st x
        return! fold f st xs 
    | [] -> return st }

