module TheGamma.TypePoviders

open TheGamma.Babel
open Fable.Import
open Fable.Extensions

type ProvidedType = 
  | NamedType of string * Type
  | GlobalValue of string * Expression * Type

// ------------------------------------------------------------------------------------------------
// F# provider
// ------------------------------------------------------------------------------------------------

module FSharpProvider = 
  type AnyType = 
    { kind : string }

  type GenericParameterType = 
    { kind : string 
      name : string }

  type PrimitiveType = 
    { kind : string 
      name : string }

  type FunctionType = 
    { kind : string 
      arguments : AnyType[]
      returns : AnyType }

  type NamedType = 
    { kind : string 
      name : string
      typargs : AnyType[] }
  
  type Member = 
    { kind : string }

  type Argument = 
    { name : string
      optional : bool
      ``type`` : AnyType }

  type MethodMember = 
    { kind : string
      name : string 
      typepars : AnyType[]
      arguments:Argument[]
      returns : AnyType }

  type PropertyMember = 
    { kind : string
      name : string 
      returns : AnyType }

  type ExportedType = 
    { typepars : AnyType
      ``static`` : bool 
      instance : string[]
      members : Member[] }


  let provideFSharpType lookupNamed url = 

    let rec mapType (t:AnyType) = 
      match t.kind with
      | "primitive" -> Type.Primitive (unbox<PrimitiveType> t).name
      | "function"->
          let t = unbox<FunctionType> t
          Type.Function(List.ofSeq (Array.map mapType t.arguments),mapType t.returns)
      | "named" -> 
          let t = (unbox<NamedType> t)
          lookupNamed t.name (Array.map mapType t.typargs)
      | "parameter" -> Type.Parameter (unbox<GenericParameterType> t).name
      | _ -> failwith "provideFSharpType: Unexpected type"

    async {
      let! json = Http.Request("GET", url, None)
      let expTys = jsonParse<ExportedType[]> json
      return
        [ for exp in expTys ->
            let mems = 
              [ for m in exp.members do
                  if m.kind = "method" then
                    let m = unbox<MethodMember> m
                    let args = [ for a in m.arguments -> a.name, a.optional, mapType a.``type`` ]
                    let emitter = { Emit = fun (inst, args) ->
                      // TODO: match arguments based on name or something
                      CallExpression
                        ( MemberExpression(inst, IdentifierExpression(m.name, None), false, None), 
                          List.map snd args, None) }
                    yield Member.Method(m.name, args, mapType m.returns, emitter) ]
            
            if exp.``static`` then
            
            Type.Object { Members = mems } ] }
    

// ------------------------------------------------------------------------------------------------
// REST provider
// ------------------------------------------------------------------------------------------------

module RestProvider = 

  type Type = { kind:string }
  type TypeNested = { kind:string (* = nested *); endpoint:string }
  type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

  [<Fable.Core.Emit("typeof($0)")>]
  let jstypeof (o:obj) : string = failwith "!"

  type Member =
    { name : string
      returns : Type
      trace : string[] }

  type ResultType = 
    | Primitive of string
    | Generic of string * ResultType list
    | Record of (string * ResultType) list

  type RawField = 
    { name : string
      ``type`` : obj }

  type RawResultType = 
    { name : string 
      fields : RawField[]
      ``params`` : obj[] }

  let rec fromRawType (json:obj) =
    if jstypeof json = "string" then Primitive(unbox json)
    else
      let res = unbox<RawResultType> json
      if res.name = "record" then Record [ for f in res.fields -> f.name, fromRawType f.``type`` ]
      else Generic(res.name, [ for f in res.``params`` -> fromRawType f ])
 
  let load url = async {
    let! json = Http.Request("GET", url, None)
    let members = jsonParse<Member[]> json
    return members }

  let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
  let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

  let concatUrl (a:string) (b:string) =
    (trimRight '/' a) + "/" + (trimLeft '/' b)

  let propAccess trace = 
    let trace = StringLiteral(String.concat "&" trace, None)
    { Emit = fun (inst, _args) ->
        let mem = MemberExpression(inst, IdentifierExpression("addTrace", None), false, None)
        CallExpression(mem, [trace], None) }

  let dataCall parser trace endp = 
    { Emit = fun (inst, args) ->
        let tr = (propAccess trace).Emit(inst, args) 
        let mem = MemberExpression(tr, IdentifierExpression("getValue", None), false, None)
        CallExpression(mem, [StringLiteral(endp, None)], None) |> parser }

  let ident s = IdentifierExpression(s, None)
  let str v = StringLiteral(v, None)
  let (?) (e:Expression) (s:string) = MemberExpression(e, IdentifierExpression(s, None), false, None)
  let (@) (e:Expression) (args) = CallExpression(e, args, None)

  // Turn "Async<string>" into the required type (haha)
  let getTypeAndEmitter lookupNamed ty = 
    match ty with
    | Generic("seq", [Generic("tuple", [t1; t2])]) -> 
        let typ = lookupNamed "series" [] // TODO: Generics
        typ, 
        fun d -> ident("_series")?series?create @ [d; str "key"; str "value"; str "series"] // TODO: We don't have any info - that sucks
    | _ -> failwith "Nop"
    
  let rec createRestType lookupNamed root url = 
    async {
      let! members = load (concatUrl root url)
      return 
        Type.Object
          { Members = 
              [ for m in members ->
                  match m.returns.kind with
                  | "nested" ->
                      let returns = unbox<TypeNested> m.returns 
                      Member.Property(m.name, createRestType lookupNamed root returns.endpoint, propAccess m.trace) 
                  | "primitive" ->  
                      let returns = unbox<TypePrimitive> m.returns                      
                      let ty = fromRawType returns.``type``
                      let typ, parser = getTypeAndEmitter lookupNamed ty
                      Member.Property(m.name, typ, dataCall parser m.trace returns.endpoint)
                  | _ -> failwith "?" ] } }
    |> Async.AsFuture |> Type.Delayed

  let rec provideRestType lookupNamed root = 
    let ctx = 
      MemberExpression
        ( IdentifierExpression("_restruntime", None), 
          IdentifierExpression("RuntimeContext", None), false, None)
    NewExpression(ctx, [StringLiteral(root, None); StringLiteral("", None)], None),
    createRestType lookupNamed root "/"

  // ------------------------------------------------------------------------------------------------
  //
  // ------------------------------------------------------------------------------------------------
  (*
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

        *)