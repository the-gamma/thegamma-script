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
    { name : string
      typepars : AnyType
      ``static`` : bool 
      instance : string[]
      members : Member[] }


  let provideFSharpTypes lookupNamed url = 

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

    // Needs to be delayed to avoid calling lookupNamed too early
    let importProvidedType exp = async {
      let mems = 
        exp.members |> Array.choose (fun m ->
          if m.kind = "method" then
            let m = unbox<MethodMember> m
            let args = [ for a in m.arguments -> a.name, a.optional, mapType a.``type`` ]
            let emitter = { Emit = fun (inst, args) ->
              // TODO: match arguments based on name or something
              CallExpression
                ( MemberExpression(inst, IdentifierExpression(m.name, None), false, None), 
                  List.map snd args, None) }
            Some(Member.Method(m.name, args, mapType m.returns, emitter))
          else None)
      return Type.Object { Members = mems } } |> Async.AsFuture 
            
    async {
      let! json = Http.Request("GET", url)
      let expTys = jsonParse<ExportedType[]> json
      return
        [ for exp in expTys ->
            let guid = url + "," + exp.name
            let ty = Type.Delayed(guid, importProvidedType exp)
            if exp.``static`` then           
              let e = exp.instance |> Seq.fold (fun chain s -> 
                match chain with
                | None -> Some(IdentifierExpression(s, None))
                | Some e -> Some(MemberExpression(e, IdentifierExpression(s, None), false, None)) ) None |> Option.get
              ProvidedType.GlobalValue(exp.name, e, ty)
            else
              ProvidedType.NamedType(exp.name, ty) ] }
    

// ------------------------------------------------------------------------------------------------
// REST provider
// ------------------------------------------------------------------------------------------------

module RestProvider = 

  type AnyType = { kind:string }
  type TypeNested = { kind:string (* = nested *); endpoint:string }
  type TypePrimitive = { kind:string (* = primitive *); ``type``:obj; endpoint:string }

  [<Fable.Core.Emit("typeof($0)")>]
  let jstypeof (o:obj) : string = failwith "!"

  type Parameter = 
    { name : string 
      ``type`` : string }

  type Member =
    { name : string
      returns : AnyType
      parameters : Parameter[] option
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
    let mem = MemberExpression(inst, IdentifierExpression("addTrace", None), false, None)
    CallExpression(mem, [trace], None)

  let propAccess trace = 
    let trace = StringLiteral(String.concat "&" trace, None)
    { Emit = fun (inst, _args) -> addTraceCall inst trace }

  let methCall trace =
    let trace = StringLiteral(String.concat "&" trace, None)
    { Emit = fun (inst, args) ->
        let withTrace = addTraceCall inst trace
        args |> Seq.fold (fun inst (name, value) ->
          let trace = BinaryExpression(BinaryPlus, StringLiteral(name + "=", None), value, None)
          addTraceCall inst trace ) withTrace }

  let dataCall parser trace endp = 
    { Emit = fun (inst, args) ->
        let tr = (propAccess trace).Emit(inst, args) 
        let mem = MemberExpression(tr, IdentifierExpression("getValue", None), false, None)
        CallExpression(mem, [StringLiteral(endp, None)], None) |> parser }

  let ident s = IdentifierExpression(s, None)
  let str v = StringLiteral(v, None)
  let (?) (e:Expression) (s:string) = MemberExpression(e, IdentifierExpression(s, None), false, None)
  let (/@/) (e:Expression) (args) = CallExpression(e, args, None)
  let func v f = 
    let body = BlockStatement([ReturnStatement(f (ident v), None)], None)
    FunctionExpression(None, [IdentifierPattern(v, None)], body, false, false, None)

//  Member.Property
//  Type.Object ObjectType


  // Turn "Async<string>" into the required type
  // I guess we should keep a flag whether the input is still async (or something)
  let rec getTypeAndEmitter (lookupNamed:string -> TheGamma.Type list -> TheGamma.Type) ty = 
    match ty with
    | Primitive("string") -> Type.Primitive("string"), id
    | Primitive("int") -> Type.Primitive("num"), id
    | Primitive("float") -> Type.Primitive("num"), id
    | Generic("seq", [|Generic("tuple", [|t1; t2|])|]) -> 
        // let t1, _ = getTypeAndEmitter lookupNamed t1
        // let t2, _ = getTypeAndEmitter lookupNamed t2
        let typ = lookupNamed "series" [] // TODO: Generics 
        typ, 
        fun d -> ident("_series")?series?create /@/ [d; str "key"; str "value"; str "series"] // TODO: We don't have any info - that sucks
    | Generic("seq", [|ty|]) ->
        let elTy, emitter = getTypeAndEmitter lookupNamed ty
        let serTy = lookupNamed "series" [Type.Primitive "int"; elTy]
        serTy, 
        // This is over async, but the child `emitter` is not over async
        fun d -> 
          ident("_series")?series?ordinal /@/ 
            [ ident("_restruntime")?convertSequence /@/ [func "v" emitter; d] 
              str "key"; str "value"; str "series" ]
    | Record(membs) ->
        let membs = 
          membs |> Array.map (fun (name, ty) ->
            let memTy, memConv = getTypeAndEmitter lookupNamed ty
            let emitter = { Emit = fun (inst, _) -> memConv <| inst?(name) }
            Member.Property(name, memTy, None, emitter))
        let obj = TheGamma.Type.Object { Members = membs }
        obj, id
    | _ -> 
        Browser.console.log("getTypeAndEmitter: Cannot handle %O", ty)
        failwith "getTypeAndEmitter: Cannot handle type"

  [<Fable.Core.Emit("$0[$1]")>]
  let getProperty<'T> (obj:obj) (name:string) : 'T = failwith "never"

  let rec createRestType lookupNamed root cookies url = 
    let future = async {
      let! members = load (concatUrl root url) cookies 
      return 
        Type.Object
          { Members = 
              members |> Array.map (fun m ->
                let schema = m.schema |> Option.map (fun s -> { Type = getProperty s "@type"; JSON = s })
                match m.returns.kind with
                | "nested" ->
                    let returns = unbox<TypeNested> m.returns 
                    let retTyp = createRestType lookupNamed root cookies returns.endpoint
                    match m.parameters with 
                    | Some parameters ->
                        let args = [ for p in parameters -> p.name, false, Type.Primitive p.``type``] // TODO: Check this is OK type
                        let argNames = [ for p in parameters -> p.name ]
                        Member.Method(m.name, args, retTyp, methCall m.trace)
                    | None -> 
                        Member.Property(m.name, retTyp, schema, propAccess m.trace) 
                | "primitive" ->  
                    let returns = unbox<TypePrimitive> m.returns                      
                    let ty = fromRawType returns.``type``
                    let typ, parser = getTypeAndEmitter lookupNamed ty
                    Member.Property(m.name, typ, schema, dataCall parser m.trace returns.endpoint)
                | _ -> failwith "?" ) } }
    let guid = concatUrl root url
    Type.Delayed(guid, Async.AsFuture(future))

  let rec provideRestType lookupNamed name root cookies = 
    let ctx = ident("_restruntime")?RuntimeContext
    ProvidedType.GlobalValue
      ( name, 
        NewExpression(ctx, [str root; str cookies; str ""], None),
        createRestType lookupNamed root cookies "/")

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