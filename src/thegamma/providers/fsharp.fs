module TheGamma.TypeProviders.FSharpProvider

open TheGamma
open TheGamma.Babel
open TheGamma.Common
open Fable.Import
open ProviderHelpers

// ------------------------------------------------------------------------------------------------
// F# provider
// ------------------------------------------------------------------------------------------------

type AnyType = obj
  //{ kind : string }
  
[<Emit("$0.kind")>]
let getKind (o:AnyType) : string = 
  o.GetType().GetProperty("kind").GetValue(o) :?> string

type GenericParameterType = 
  { kind : string 
    name : string }

type ArrayType = 
  { kind : string 
    element : AnyType }

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
  
type Member = obj
  //{ kind : string }

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
    typepars : AnyType[]
    ``static`` : bool 
    instance : string[]
    members : Member[] }

type GenericType =
  inherit ObjectType
  abstract TypeArguments : Type list
  abstract IsInstanceOfSchema : GenericTypeSchema -> bool

and GenericTypeSchema = 
  inherit ObjectType
  abstract TypeArguments : TypeSchema list
  abstract Substitute : (string -> Type option) -> GenericType

and GenericTypeDefinition = 
  inherit ObjectType
  abstract TypeParameterCount : int
  abstract Apply : TypeSchema list -> GenericTypeSchema

and TypeSchema = 
  | Primitive of Type
  | GenericType of GenericTypeSchema
  | Parameter of string
  | Function of TypeSchema list * TypeSchema
  | List of TypeSchema

let rec unifyTypes ctx schemas tys = 
  match schemas, tys with
  | [], [] -> Some ctx
  | TypeSchema.GenericType(gs)::ss, Type.Object(:? GenericType as gt)::ts 
      when gt.IsInstanceOfSchema gs && List.length gs.TypeArguments = List.length gt.TypeArguments ->
      unifyTypes ctx (gs.TypeArguments @ ss) (gt.TypeArguments @ ts)
  | TypeSchema.Primitive(t1)::ss, t2::ts when Types.typesEqual t1 t2 -> unifyTypes ctx ss ts
  | TypeSchema.Parameter(n)::ss, t::ts -> unifyTypes ((n,t)::ctx) ss ts
  | TypeSchema.List(s)::ss, Type.List(t)::ts -> unifyTypes ctx (s::ss) (t::ts)
  | TypeSchema.Function(sa, sr)::ss, Type.Function(ta, tr)::ts when List.length sa = List.length ta -> 
      unifyTypes ctx (sr::(sa @ ss)) (tr::(ta @ ts)) 
  | TypeSchema.GenericType(_)::_, _ 
  | TypeSchema.Primitive(_)::_, _ 
  | TypeSchema.List(_)::_, _ 
  | TypeSchema.Function(_)::_, _ 
  | [], _
  | _, [] -> None

let rec substituteTypeParams assigns schema = 
  match schema with
  | TypeSchema.GenericType ts -> Type.Object(ts.Substitute assigns)
  | TypeSchema.Primitive t -> t
  | TypeSchema.List s -> Type.List (substituteTypeParams assigns s)
  | TypeSchema.Parameter n -> match assigns n with Some t -> t | _ -> failwith "substituteTypeParams: unresolved type parameter"
  | TypeSchema.Function(is, rs) -> 
      Type.Function(List.map (substituteTypeParams assigns) is, substituteTypeParams assigns rs)

let rec partiallySubstituteTypeParams (assigns:string -> Type option) schema = 
  match schema with
  | TypeSchema.Primitive t -> TypeSchema.Primitive t
  | TypeSchema.List s -> TypeSchema.List (partiallySubstituteTypeParams assigns s)
  | TypeSchema.Parameter n when (assigns n).IsSome -> TypeSchema.Primitive(assigns(n).Value) 
  | TypeSchema.Parameter n -> TypeSchema.Parameter n
  | TypeSchema.Function(is, rs) -> 
      TypeSchema.Function
        ( List.map (partiallySubstituteTypeParams assigns) is, 
          partiallySubstituteTypeParams assigns rs )
  | TypeSchema.GenericType ts ->
      { new GenericTypeSchema with
          member x.Members = failwith "Uninstantiated generic type schema"
          member x.TypeEquals _ = failwith "Uninstantiated generic type schema"
          member x.TypeArguments = List.map (partiallySubstituteTypeParams assigns) ts.TypeArguments
          member x.Substitute assigns2 =
            ts.Substitute (fun n ->
              match assigns2 n, assigns n with
              | Some t, _ 
              | _, Some t -> Some t
              | _ -> None) } |> TypeSchema.GenericType    
   

// Needs to be delayed to avoid calling lookupNamed too early
let importProvidedType lookupNamed exp = fun () ->
  let rec mapType (t:AnyType) = 
    match getKind t with
    | "primitive" -> 
        ( match (unbox<PrimitiveType> t).name with
          | "object" -> Type.Any
          | "int" | "float" -> Type.Primitive PrimitiveType.Number
          | "string" -> Type.Primitive PrimitiveType.String
          | "bool" -> Type.Primitive PrimitiveType.Bool
          | "unit" -> Type.Primitive PrimitiveType.Unit
          | t -> failwith ("provideFSharpType: Unsupported type: " + t) )
        |> TypeSchema.Primitive
    | "function"->
        let t = unbox<FunctionType> t
        TypeSchema.Function(List.ofSeq (Array.map mapType t.arguments),mapType t.returns)
    | "named" -> 
        let t = (unbox<NamedType> t)
        let tyargs = List.ofArray (Array.map mapType t.typargs)
        match lookupNamed t.name with
        | Type.Object (:? GenericTypeDefinition as gtd) -> 
            if gtd.TypeParameterCount <> List.length tyargs then 
              failwith "provideFSharpType: Named type has mismatching nuumber of arguments"
            gtd.Apply tyargs |> TypeSchema.GenericType 
        | t -> TypeSchema.Primitive t
    | "parameter" -> TypeSchema.Parameter (unbox<GenericParameterType> t).name
    | "array" -> TypeSchema.List(mapType (unbox<ArrayType> t).element)
    | _ -> failwith "provideFSharpType: Unexpected type"

  let getTypeParameters typars = 
    typars |> Array.map (fun t -> 
      match mapType t with
      | TypeSchema.Parameter(n) -> n
      | _ -> failwith "importProvidedType: expected type parameter") |> List.ofArray

  let generateMembers assigns = 
    exp.members |> Array.choose (fun m ->
      if getKind m = "method" then
        let m = unbox<MethodMember> m
        let typars = getTypeParameters m.typepars 

        let args = [ for a in m.arguments -> a.name, a.optional, partiallySubstituteTypeParams assigns (mapType a.``type``) ]
        let emitter = { Emit = fun (inst, args) ->
          CallExpression
            ( MemberExpression(inst, IdentifierExpression(m.name, None), false, None), 
              args, None) }
            
        let retTyp = mapType m.returns
        let retFunc tys =
          match unifyTypes [] [ for _, _, t in args -> t ] tys with 
          | None -> None
          | Some assigns ->
              let assigns =
                assigns 
                |> Seq.groupBy fst
                |> Seq.map (fun (p, tys) ->
                    p, tys |> Seq.fold (fun st (_, ty2) ->
                      match st with
                      | Some ty1 -> if Types.typesEqual ty1 ty2 then Some ty1 else None
                      | None -> None) (Some Type.Any) )
                |> Seq.fold (fun assigns assign ->
                  match assigns, assign with
                  | Some assigns, (p, Some assign) -> Some ((p,assign)::assigns)
                  | _ -> None) (Some [])
              match assigns with
              | Some assigns when List.length assigns = List.length typars ->
                  let assigns = dict assigns
                  let subst n = if assigns.ContainsKey n then Some assigns.[n] else None
                  Some (substituteTypeParams subst retTyp)
              | _ -> None

        // How to show type parameters before they are eliminated?
        let args = [ for n, o, t in args -> n, o, substituteTypeParams (fun _ -> Some Type.Any) t ] 
        Some(Member.Method(m.name, args, retFunc, [docMeta (Documentation.Text "")], emitter))
      else None)

  let objectType = 
    match getTypeParameters exp.typepars with
    | [] -> 
        { new ObjectType with
            member x.Members = generateMembers (fun _ -> None) 
            member x.TypeEquals _ = false }
    | typars ->
        { new GenericTypeDefinition with
            member x.TypeParameterCount = List.length typars
            member x.Members = failwith "Uninstantiated generic type definition"
            member x.TypeEquals _ = failwith "Uninstantiated generic type definition"
            member x.Apply typars = 
              { new GenericTypeSchema with
                  member x.Members = failwith "Uninstantiated generic type schema"
                  member x.TypeEquals _ = failwith "Uninstantiated generic type schema"
                  member x.Substitute assigns = 
                    { new GenericType with
                        member x.TypeArguments = List.map (substituteTypeParams assigns ) typars
                        member x.IsInstanceOfSchema sch = failwith "TODO"
                        member x.Members = generateMembers assigns
                        member x.TypeEquals _ = failwith "TODO" }
                  member x.TypeArguments = typars } } :> _
    
  objectType |> Type.Object

let provideFSharpTypes lookupNamed url =   
  async {
    let! json = Http.Request("GET", url)
    let expTys = jsonParse<ExportedType[]> json
    return
      [ for exp in expTys ->
          let ty = 
            Type.Delayed(Async.CreateNamedFuture exp.name <| async {
              return importProvidedType lookupNamed exp () })
          if exp.``static`` then           
            let e = exp.instance |> Seq.fold (fun chain s -> 
              match chain with
              | None -> Some(IdentifierExpression(s, None))
              | Some e -> Some(MemberExpression(e, IdentifierExpression(s, None), false, None)) ) None |> Option.get
            let ty = 
              match ty with
              | Type.Object(:? GenericTypeDefinition as gtd) ->
                  let ts = [ for i in 1 .. gtd.TypeParameterCount -> TypeSchema.Primitive Type.Any ]
                  Type.Object(gtd.Apply(ts).Substitute(fun _ -> None))
              | _ -> ty
            ProvidedType.GlobalValue(exp.name, [], e, ty)
          else
            ProvidedType.NamedType(exp.name, ty) ] }
    