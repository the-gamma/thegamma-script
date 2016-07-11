module TheGamma.TypePoviders

// ------------------------------------------------------------------------------------------------
// REST provider
// ------------------------------------------------------------------------------------------------

open TheGamma.Babel

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

let propAccess trace = 
  let trace = [ for s in trace -> StringLiteral(s, None) ]
  { Emit = fun (inst, args) ->
      let mem = MemberExpression(inst, IdentifierExpression("concat", None), false, None)
      CallExpression(mem, [ArrayExpression(trace, None)], None) }

let dataCall trace endp = 
  { Emit = fun (inst, args) ->
      let tr = (propAccess trace).Emit(inst, args) 
      CallExpression(IdentifierExpression("download", None), [StringLiteral(endp, None); tr], None) }

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
                    Member.Property(m.name, createRestType root returns.endpoint, propAccess m.trace) 
                | "primitive" ->  
                    let returns = unbox<TypePrimitive> m.returns
                    Member.Property(m.name, Type.Primitive "num", dataCall m.trace returns.endpoint) 
                | _ -> failwith "?" ] } }
  |> Async.AsFuture |> Type.Delayed


// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

let nada = { Emit = fun (inst, args) -> Babel.NullLiteral(None) }

let rec seriesTy() = 
  { new Future<_> with
      member x.Then(f) = 
        Type.Object 
          { Members = 
            [ Member.Method("sortValues", ["reverse", Type.Primitive "bool"], seriesTy (), nada)
              Member.Method("take", ["count", Type.Primitive "num"], seriesTy (), nada) ] } |> f } |> Type.Delayed

let worldTy = 
  Type.Object
    { Members = 
        [ Member.Property("CO2 emissions (kt)", seriesTy (), nada) ] }

