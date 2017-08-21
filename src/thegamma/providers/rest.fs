// ------------------------------------------------------------------------------------------------
// REST type provider
// ------------------------------------------------------------------------------------------------
module TheGamma.TypeProviders.RestProvider

open TheGamma
open TheGamma.Babel
open TheGamma.Babel.BabelOperators
open TheGamma.Common
open TheGamma.TypeProviders.ProviderHelpers
open Fable.Import

// ------------------------------------------------------------------------------------------------
// Types to represent JSON data returned by REST service
// ------------------------------------------------------------------------------------------------

type AnyType = { kind:string }
type TypeNested = { kind:string (* = nested *); endpoint:string }
type TypeProvider = { kind:string (* = provider *); provider:string; endpoint:string }
type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

[<Fable.Core.Emit("typeof($0)")>]
let jstypeof (o:obj) : string = failwith "!"

type Documentation = 
  { title : string option
    details : string option }

type Parameter =
  { name : string
    optional : bool
    kind : string
    cookie : string option
    trace : string option
    ``type`` : obj }

type Member =
  { name : string
    returns : AnyType
    parameters : Parameter[] option
    documentation : obj option
    schema : obj[]
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

// ------------------------------------------------------------------------------------------------
// Code generation for provided members
// ------------------------------------------------------------------------------------------------
 
let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

let load url cookies = async {
  let! json = Http.Request("GET", url, cookies=cookies)
  let members = jsonParse<Member[]> json
  return members }

let addTraceCall inst trace =
  if Seq.isEmpty trace then inst 
  else inst?addTrace /@/ [str (String.concat "&" trace)]

let propAccess trace = 
  { Emit = fun inst -> addTraceCall inst trace }

let methCall traceNames trace =
  { Emit = fun inst -> funcN (Seq.length traceNames) (fun args ->
      let withTrace = addTraceCall inst trace
      Seq.zip traceNames args |> Seq.fold (fun inst (name, value) ->
        let trace = BinaryExpression(BinaryPlus, str(name + "="), value, None)
        inst?addTrace /@/ [trace] ) withTrace) }

let dataCall parser trace endp = 
  { Emit = fun inst ->
      let tr = (propAccess trace).Emit(inst) 
      let mem = MemberExpression(tr, IdentifierExpression("getValue", None), false, None)
      CallExpression(mem, [StringLiteral(endp, None)], None) |> parser }
 

// Turn "Async<string>" into the required type
// I guess we should keep a flag whether the input is still async (or something)
let rec getTypeAndEmitter (lookupNamed:string -> Type) ty = 
  match ty with
  | Primitive("string") -> Type.Primitive(PrimitiveType.String), id
  | Primitive("obj") -> Type.Primitive(PrimitiveType.String), id
  | Primitive("int") 
  | Primitive("float") -> Type.Primitive(PrimitiveType.Number), fun e -> ident("Number") /@/ [e]
  | Primitive("date") -> Type.Primitive(PrimitiveType.Date), fun e -> NewExpression(ident("Date"), [ident("Date")?parse /@/ [e]], None)
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
          let emitter = { Emit = fun inst -> memConv <| inst?(name) }
          { Member.Name = name; Type = memTy; Metadata = [docMeta(Documentation.Text "")]; Emitter = emitter })
      let obj = 
        { new ObjectType with
            member x.Members = membs
            member x.TypeEquals _ = false }
        |> TheGamma.Type.Object 
      obj, id
  | _ -> 
      Browser.console.log("getTypeAndEmitter: Cannot handle %O", ty)
      failwith "getTypeAndEmitter: Cannot handle type"

// ------------------------------------------------------------------------------------------------
// Type provider
// ------------------------------------------------------------------------------------------------

let restTypeCache = System.Collections.Generic.Dictionary<_, _>()

let rec createRestType lookupNamed resolveProvider root cookies url = 

  let provideMember m = 
    let schema = 
      if m.schema = null then []
      elif isArray m.schema then m.schema |> Array.map (fun s ->
        { Type = getProperty s "@type"; Context = getProperty s "@context"; Data = s }) |> List.ofSeq
      else 
        [ { Type = getProperty m.schema "@type"; Context = getProperty m.schema "@context"; Data = m.schema } ]

    match m.returns.kind with
    | "provider" ->
        let returns = unbox<TypeProvider> m.returns 
        let typ, emitter = resolveProvider returns.provider returns.endpoint
        { Member.Name = m.name; Type = typ; Metadata = (docMeta (parseDoc m.documentation))::schema; Emitter = emitter }
    | "nested" ->
        let returns = unbox<TypeNested> m.returns 
        let createReturnType cookies = 
          try Some(createRestType lookupNamed resolveProvider root cookies returns.endpoint)
          with _ -> None

        match m.parameters with 
        | Some parameters ->
            let args = 
              [ for p in parameters -> 
                  let ty = fromRawType p.``type``
                  let ty, _ = getTypeAndEmitter lookupNamed ty
                  { MethodArgument.Name = p.name; Optional = p.optional; Type = ty; Static = p.kind = "static" } ] 
            
            let retFunc tys = 
              if not (Types.listsEqual (List.map fst tys) [ for ma in args -> ma.Type ] Types.typesEqual) then None else
              let matched = Seq.zip parameters tys
              let newCookies = 
                matched |> Seq.choose (function
                  | pa, (_, Some value) when pa.kind = "static" -> Some(pa.cookie.Value + "=" + Fable.Import.JS.encodeURIComponent(string value))
                  | _ -> None) 
              let cookies = Seq.append [cookies] newCookies |> String.concat "&"
              createReturnType cookies

            let traceNames = parameters |> Seq.choose (fun p -> p.trace)
            { Member.Name = m.name; Metadata = [docMeta (parseDoc m.documentation)]
              Type = Type.Method(args, retFunc); Emitter = methCall traceNames m.trace }
        | None -> 
            let retTyp = defaultArg (createReturnType cookies) Type.Any
            { Member.Name = m.name; Type = retTyp; Metadata = (docMeta (parseDoc m.documentation))::schema; Emitter = propAccess m.trace }
    | "primitive" ->  
        let returns = unbox<TypePrimitive> m.returns                      
        let ty = fromRawType returns.``type``
        let typ, parser = getTypeAndEmitter lookupNamed ty
        { Member.Name = m.name; Type = typ; Metadata = (docMeta (parseDoc m.documentation))::schema; 
          Emitter = dataCall parser m.trace returns.endpoint }
    | _ -> failwith "?" 

  let guid = (concatUrl root url) + cookies
  match restTypeCache.TryGetValue guid with
  | true, res -> res
  | _ ->
    let future = async {
      try
        let! members = load (concatUrl root url) cookies 
        let members = members |> Array.map provideMember
        return 
          { new ObjectType with 
              member x.Members = members
              member x.TypeEquals _ = false } |> Type.Object 
      with e -> 
        Log.error("providers", "Cannot provide object type: %O", e)
        return Type.Any }
    let ty = Type.Delayed(Async.CreateNamedFuture guid future)
    restTypeCache.[guid] <- ty
    ty

let rec provideRestType lookupNamed resolveProvider name root cookies = 
  let ctx = ident("RuntimeContext")
  ProvidedType.GlobalValue
    ( name, [],
      NewExpression(ctx, [str root; str cookies; str ""], None),
      createRestType lookupNamed resolveProvider root cookies "/")