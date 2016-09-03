module TheGamma.TypePovidersRuntime
open System
open Fable.Extensions

let convertTupleSequence f g data = async {
  let! values = data
  return values |> Array.map (fun (a, b) -> f a, g b) }

let convertSequence f data = async {
  let! values = data
  return values |> Array.map f }

let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

type RuntimeContext(root:string, cookies:string, trace:string) = 
  member x.root = root
  member x.trace = trace
  
  member x.addTrace(suffix) = 
    let traces = 
      [ if not (String.IsNullOrEmpty trace) then yield trace
        if not (String.IsNullOrEmpty suffix) then yield suffix ]
    RuntimeContext(root, cookies, String.concat "&" traces)
  
  member x.getValue(endpoint:string) =     
    async { 
      let! res = Http.Request("POST", concatUrl root endpoint, trace, cookies)
      // TODO: This is wrong - it may return an integer too!
      return jsonParse<obj> res }

type PivotContext(root, calls) = 
  member x.addCall(callid:string, values:obj[]) =
    PivotContext(root, Array.append [| callid, values |] calls)
  member x.getData(tfs:string) = async {
    let url = calls |> Array.fold (fun (tfs:string) (id, vals) -> 
      if vals.Length <> 1 then failwith "PivotContext.getData: Expected one argument"
      tfs.Replace(id, string vals.[0])) tfs
    Log.trace("runtime", "Pivot: %s", concatUrl root url)
    let! res = Http.Request("GET", concatUrl root url)
    return jsonParse<obj> res }
