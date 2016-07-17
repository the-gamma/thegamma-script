module TheGamma.TypePovidersRuntime
open System
open Fable.Extensions

let convertSequence f data = async {
  let! values = data
  return values |> Array.map f }

let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

type RuntimeContext(root:string, trace:string) = 
  member x.root = root
  member x.trace = trace
  
  member x.addTrace(suffix) = 
    let traces = 
      [ if not (String.IsNullOrEmpty trace) then yield trace
        if not (String.IsNullOrEmpty suffix) then yield suffix ]
    RuntimeContext(root, String.concat "&" traces)
  
  member x.getValue(endpoint:string) =     
    async { 
      let! res = Http.Request("POST", concatUrl root endpoint, Some trace)
      // TODO: This is wrong - it may return an integer too!
      return jsonParse<obj> res }



