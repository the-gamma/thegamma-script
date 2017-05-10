module TheGamma.TypeProviders.RestProvider
(*
open TheGamma
open TheGamma.Babel
open TheGamma.Common
open TheGamma.TypeProviders.ProviderHelpers
open Fable.Import

// ------------------------------------------------------------------------------------------------
// REST provider
// ------------------------------------------------------------------------------------------------

type AnyType = { kind:string }
type TypeNested = { kind:string (* = nested *); endpoint:string }
type TypeProvider = { kind:string (* = provider *); provider:string; endpoint:string }
type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

[<Fable.Core.Emit("typeof($0)")>]
let jstypeof (o:obj) : string = failwith "!"

type Parameter = 
  { name : string 
    ``type`` : string }

type Documentation = 
  { title : string option
    details : string option }

type Member =
  { name : string
    returns : AnyType
    parameters : Parameter[] option
    documentation : obj option // This can be Documentation or string or an endpoint
    schema : obj option
    trace : string[] }

type ResultType = 
  | Primitive of string
  | Generic of string * ResultType[]
  | Record of (string * ResultType)[]

type RawField = 
  { name : string
    ``type`` : obj }

type RawResultType = 
  { name : string 
    fields : RawField[]
    ``params`` : obj[] }

let parseDoc (json:obj option) =
  if json.IsNone then Documentation.None
  elif jstypeof json.Value = "string" then Documentation.Text(unbox json)
  else 
    let doc = unbox<Documentation> json.Value
    match doc.title, doc.details with 
    | Some title, Some dets -> Documentation.Details(title, dets)
    | _ -> Documentation.None

let rec fromRawType (json:obj) =
  if jstypeof json = "string" then Primitive(unbox json)
  else
    let res = unbox<RawResultType> json
    if res.name = "record" then res.fields |> Array.map (fun f -> f.name, fromRawType f.``type``) |> Record
    else Generic(res.name, res.``params`` |> Array.map fromRawType)
 
let load url cookies = async {
  let! json = Http.Request("GET", url, cookies=cookies)
  let members = jsonParse<Member[]> json
  return members }

let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

let addTraceCall inst trace =
  if Seq.isEmpty trace then inst 
  else
    let trace = StringLiteral(String.concat "&" trace, None)    
    let mem = MemberExpression(inst, IdentifierExpression("addTrace", None), false, None)
    CallExpression(mem, [trace], None)

let propAccess trace = 
  { Emit = fun (inst, _args) -> addTraceCall inst trace }

let methCall argNames trace =
  { Emit = fun (inst, args) ->
      let withTrace = addTraceCall inst trace
      Seq.zip argNames args |> Seq.fold (fun inst (name, value) ->
        let trace = BinaryExpression(BinaryPlus, StringLiteral(name + "=", None), value, None)
        let mem = MemberExpression(inst, IdentifierExpression("addTrace", None), false, None)
        CallExpression(mem, [trace], None) ) withTrace }

let dataCall parser trace endp = 
  { Emit = fun (inst, args) ->
      let tr = (propAccess trace).Emit(inst, args) 
      let mem = MemberExpression(tr, IdentifierExpression("getValue", None), false, None)
      CallExpression(mem, [StringLiteral(endp, None)], None) |> parser }

// Turn "Async<string>" into the required type
// I guess we should keep a flag whether the input is still async (or something)
let rec getTypeAndEmitter (lookupNamed:string -> TheGamma.Type) ty = 
  match ty with
  | Primitive("string") -> Type.Primitive(PrimitiveType.String), id
  | Primitive("int") 
  | Primitive("float") -> Type.Primitive(PrimitiveType.Number), fun e -> ident("Number") /@/ [e]
  | Primitive("date") -> Type.Primitive(PrimitiveType.Date), fun e -> ident("Date")?parse /@/ [e]
  | Generic("seq", [|Generic("tuple", [|t1; t2|])|]) -> 
      let t1, e1 = getTypeAndEmitter lookupNamed t1
      let t2, e2 = getTypeAndEmitter lookupNamed t2
      let typ = FSharpProvider.applyTypes (lookupNamed "series") [t1; t2]
      typ, 
      fun d -> 
        ident("series")?create /@/ 
          [ ident("convertTupleSequence") /@/ [func "v" e1; func "v" e2; d] 
            str "key"; str "value"; str "" ] // TODO: We don't have any info - that sucks
  | Generic("seq", [|ty|]) ->
      let elTy, emitter = getTypeAndEmitter lookupNamed ty
      let serTy = FSharpProvider.applyTypes (lookupNamed "series") [Type.Primitive PrimitiveType.Number; elTy]
      serTy, 
      // This is over async, but the child `emitter` is not over async
      fun d -> 
        ident("series")?ordinal /@/ 
          [ ident("convertSequence") /@/ [func "v" emitter; d] 
            str "key"; str "value"; str "" ]
  | Record(membs) ->
      let membs = 
        membs |> Array.map (fun (name, ty) ->
          let memTy, memConv = getTypeAndEmitter lookupNamed ty
          let emitter = { Emit = fun (inst, _) -> memConv <| inst?(name) }
          Member.Property(name, memTy, [docMeta(Documentation.Text "")], emitter))
      let obj = 
        { new ObjectType with
            member x.Members = membs
            member x.TypeEquals _ = false }
        |> TheGamma.Type.Object 
      obj, id
  | _ -> 
      Browser.console.log("getTypeAndEmitter: Cannot handle %O", ty)
      failwith "getTypeAndEmitter: Cannot handle type"

[<Fable.Core.Emit("$0[$1]")>]
let getProperty<'T> (obj:obj) (name:string) : 'T = failwith "never"

let mapParamType = function
  | "int" | "float" -> PrimitiveType.Number
  | _ -> failwith "mapParamType: Unsupported parameter type"

let restTypeCache = System.Collections.Generic.Dictionary<_, _>()

let rec createRestType lookupNamed resolveProvider root cookies url = 
  let guid = (concatUrl root url) + cookies
  match restTypeCache.TryGetValue guid with
  | true, res -> res
  | _ ->
    let future = async {
      let! members = load (concatUrl root url) cookies 
      let members = members |> Array.map (fun m ->
        let schema = 
          match m.schema with
          | Some s -> [{ Type = getProperty s "@type"; Context = "http://schema.org"; Data = s }]
          | _ -> []
        match m.returns.kind with
        | "provider" ->
            let returns = unbox<TypeProvider> m.returns 
            let typ, emitter = resolveProvider returns.provider returns.endpoint
            Member.Property(m.name, typ, (docMeta (parseDoc m.documentation))::schema, emitter)
        | "nested" ->
            let returns = unbox<TypeNested> m.returns 
            let retTyp = createRestType lookupNamed resolveProvider root cookies returns.endpoint
            match m.parameters with 
            | Some parameters ->
                let args = [ for p in parameters -> p.name, false, Type.Primitive (mapParamType p.``type``)] // TODO: Check this is OK type
                let argNames = [ for p in parameters -> p.name ]
                let retFunc tys = 
                  if tys = [ for _, _, t in args -> t ] then Some retTyp
                  else None
                Member.Method(m.name, args, retFunc, [docMeta (parseDoc m.documentation)], methCall argNames m.trace)
            | None -> 
                Member.Property(m.name, retTyp, (docMeta (parseDoc m.documentation))::schema, propAccess m.trace) 
        | "primitive" ->  
            let returns = unbox<TypePrimitive> m.returns                      
            let ty = fromRawType returns.``type``
            let typ, parser = getTypeAndEmitter lookupNamed ty
            Member.Property(m.name, typ, (docMeta (parseDoc m.documentation))::schema, dataCall parser m.trace returns.endpoint)
        | _ -> failwith "?" )
      return 
        { new ObjectType with 
            member x.Members = members
            member x.TypeEquals _ = false } |> Type.Object }
    let ty = Type.Delayed(Async.CreateNamedFuture guid future)
    restTypeCache.[guid] <- ty
    ty

let rec provideRestType lookupNamed resolveProvider name root cookies = 
  let ctx = ident("RuntimeContext")
  ProvidedType.GlobalValue
    ( name, [],
      NewExpression(ctx, [str root; str cookies; str ""], None),
      createRestType lookupNamed resolveProvider root cookies "/")
      *)