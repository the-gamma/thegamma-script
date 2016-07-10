module TheGamma.TypePoviders

// ------------------------------------------------------------------------------------------------
// REST provider
// ------------------------------------------------------------------------------------------------

open Fable.Extensions

type Type = { kind:string }
type TypeNested = { kind:string (* = nested *); endpoint:string }
type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

type Member =
  { name : string
    returns : Type
    trace : string[] }

let load url = async {
  let! json = Http.Request("GET", url, None)
  let members = jsonParse<Member[]> json
  return members }


let concatUrl (a:string) (b:string) =
  a.TrimEnd('/') + "/" + b.TrimStart('/')

let rec createRestType root url = 
  async {
    let! members = load (concatUrl root url)
    return 
      Type.Object
        { Members = 
            [ for m in members ->
                match m.returns.kind with
                | "nested" ->
                    let returns = unbox<TypeNested> m.returns 
                    Member.Property(m.name, createRestType root returns.endpoint) 
                | "primitive" ->
                    Member.Property(m.name, Type.Primitive "num") 
                | _ -> failwith "?" ] } }
  |> Async.AsFuture |> Type.Delayed


// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

let rec seriesTy() = 
  { new Future<_> with
      member x.Then(f) = 
        Type.Object 
          { Members = 
            [ Member.Method("sortValues", ["reverse", Type.Primitive "bool"], seriesTy ())
              Member.Method("take", ["count", Type.Primitive "num"], seriesTy ()) ] } |> f } |> Type.Delayed

let worldTy = 
  Type.Object
    { Members = 
        [ Member.Property("CO2 emissions (kt)", seriesTy ()) ] }

