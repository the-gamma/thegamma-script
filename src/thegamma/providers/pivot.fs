module TheGamma.TypeProviders.Pivot

open Fable.Core
open Fable.Import

open TheGamma
open TheGamma.Babel
open TheGamma.Common
open TheGamma.TypeProviders

// ----------------------------------------------------------------------------
// Operations that we can do on the table
// ----------------------------------------------------------------------------

type Aggregation = 
  | GroupKey
  | CountAll
  | CountDistinct of string
  | ReturnUnique of string
  | ConcatValues of string
  | Sum of string
  | Mean of string

type SortDirection =
  | Ascending
  | Descending 

type Paging =
  | Take of string
  | Skip of string
  
type Transformation = 
  | DropColumns of string list
  | SortBy of (string * SortDirection) list
  | GroupBy of string list * Aggregation list
  | FilterBy of (string * bool * string) list
  | Paging of Paging list
  | Empty
  // One of these may be the last one
  | Metadata
  | GetSeries of string * string
  | GetTheData
  | GetRange of string

type Field = 
  { Name : string 
    Type : PrimitiveType }

module Transform = 

  let private formatAgg = function
    | GroupKey -> "key"
    | CountAll -> "count-all"
    | CountDistinct(f) -> "count-dist " + Ast.escapeIdent f
    | ReturnUnique(f) -> "unique " + Ast.escapeIdent f
    | ConcatValues(f) -> "concat-vals " + Ast.escapeIdent f
    | Sum(f) -> "sum " + Ast.escapeIdent f
    | Mean(f) -> "mean " + Ast.escapeIdent f

  let toUrl transforms = 
    [ for t in transforms ->
        match t with
        | GetTheData -> []
        | Metadata -> ["metadata", []]
        | GetRange fld -> ["range", [fld]]
        | FilterBy(conds) -> ["filter", (List.map (fun (f,b,v) -> f + (if b then " eq " else " neq ") + v) conds)]
        | DropColumns(columns) -> ["drop", columns]
        | SortBy(columns) -> ["sort", (List.map (fun (c, o) -> c + (if o = Ascending then " asc" else " desc")) columns)]
        | GroupBy(flds, aggs) -> ["groupby", (List.map (fun fld -> "by " + Ast.escapeIdent fld) flds) @ (List.map formatAgg aggs)]
        | Paging(ops) -> ops |> List.map (function Take k -> "take", [k] | Skip k -> "skip", [k]) 
        | GetSeries(k, v) -> ["series", [k; v]]
        | Empty -> [] ]
    |> List.concat
    |> List.map (fun (op, args) -> 
        if List.isEmpty args then op 
        else op + "(" + String.concat "," args + ")")
    |> String.concat "$"

  (*
  let sample = 
    [ GroupBy(["Athlete"], [GroupKey; Sum("Gold Medals"); ConcatValues("Team")])
      SortBy(["Gold", Descending])
      Paging([Take "10"])
      GetSeries("Athlete", "Gold") ]
  
  toUrl  sample
  // groupby([Athlete],key,sum 'Gold Medals',concat-vals Team)$sort(Gold desc)$take(10)$series(Athlete,Gold)
  *)
   
  let singleTransformFields fields = function
    | Empty -> fields
    | Metadata -> failwith "Metadata should not appear in normal queries"
    | GetRange _ -> failwith "GetRange should not appear in normal queries"
    | GetTheData -> fields
    | SortBy _ -> fields
    | Paging _ -> fields
    | FilterBy _ -> fields
    | GetSeries(k, v) -> 
        [ fields |> List.find (fun f -> f.Name = k)
          fields |> List.find (fun f -> f.Name = v) ]
    | DropColumns(drop) ->
        let dropped = set drop
        fields |> List.filter (fun f -> not(dropped.Contains f.Name))
    | GroupBy(flds, aggs) ->
        let oldFields = dict [ for f in fields -> f.Name, f ]
        aggs 
        |> List.collect (function
           | GroupKey -> List.map (fun f -> oldFields.[f]) flds
           | ReturnUnique fld
           | ConcatValues fld
           | Sum fld -> [ oldFields.[fld] ]
           | Mean fld -> [ oldFields.[fld] ]
           | CountAll -> [ { Name = "count"; Type = PrimitiveType.Number } ]
           | CountDistinct fld -> [ { Name = oldFields.[fld].Name; Type = PrimitiveType.Number } ])
      
  let transformFields fields tfs = 
    tfs |> List.fold singleTransformFields (List.ofSeq fields) |> List.ofSeq

// ------------------------------------------------------------------------------------------------
// Pivot provider
// ------------------------------------------------------------------------------------------------

open TheGamma.TypeProviders.ProviderHelpers

let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

let makeObjectType members = 
  { new ObjectType with 
      member x.Members = Array.ofSeq members 
      member x.TypeEquals _ = false } |> Type.Object

let isNumeric fld = fld = PrimitiveType.Number
let isConcatenable fld = fld = PrimitiveType.String

// From providers.fs
let ident s = IdentifierExpression(s, None)
let str v = StringLiteral(v, None)
let bool v = BooleanLiteral(v, None)
let arr l = ArrayExpression(l, None)

let (?) (e:Expression) (s:string) = MemberExpression(e, IdentifierExpression(s, None), false, None)
let (/?/) (e:Expression) a = MemberExpression(e, a, true, None)

let (/@/) (e:Expression) (args) = CallExpression(e, args, None)
let func v f = 
  let body = BlockStatement([ReturnStatement(f (ident v), None)], None)
  FunctionExpression(None, [IdentifierPattern(v, None)], body, false, false, None)

let getTypeAndEmitter = function 
  | PrimitiveType.String -> Type.Primitive(PrimitiveType.String), id
  | PrimitiveType.Date -> Type.Primitive(PrimitiveType.String), fun e -> ident("Date")?parse /@/ [e]
  | PrimitiveType.Number -> Type.Primitive(PrimitiveType.Number), fun e -> ident "Number" /@/ [e]
  | PrimitiveType.Bool -> Type.Primitive(PrimitiveType.Number), fun e -> ident "Boolean" /@/ [e]
  | PrimitiveType.Unit -> Type.Primitive(PrimitiveType.Unit), fun e -> NullLiteral(None)

let propertyEmitter = 
  { Emit = fun (this, _) -> this }

let makeMethodEmitter callid pars =
  { Emit = fun (this, args) -> 
      let args = arr [ for v in args -> v ]
      this?addCall /@/ [str callid; args] }

let makeDataEmitter isPreview isSeries tfs = 
  { Emit = fun (this, _) -> 
      // TODO: This is not properly recursively transforming values, but they're just int/string, so it's OK
      if isSeries then
        ident("series")?create /@/ 
          [ this?getData /@/ [str (Transform.toUrl (List.rev tfs)); bool isPreview]
            str "key"; str "value"; str "" ]
      else
        ident("series")?ordinal /@/ 
          [ this?getData /@/ [str (Transform.toUrl (List.rev tfs)); bool isPreview]
            str "key"; str "value"; str "" ] }


// ----------------------------------------------------------------------------
// Transformations
// ----------------------------------------------------------------------------

type Context = 
  { Root : string
    LookupNamed : string -> Type
    InputFields : Field list
    Fields : Field list }

let rec makeProperty ctx name tfs = 
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
  Member.Property(name, makePivotType ctx tfs, [meta1; meta2], propertyEmitter)
  
and makeMethod ctx name tfs callid args = 
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
  Member.Method
    ( name, [ for n, t in args -> n, false, Type.Primitive t ], 
      (fun ts -> if ts = List.map (snd >> Type.Primitive) args then Some(makePivotType ctx tfs) else None),  
      [meta1; meta2], makeMethodEmitter callid args )

and makeDataMember ctx name isPreview tfs =
  let fields = Transform.transformFields ctx.InputFields (List.rev tfs)
  Log.trace("providers", "Make data member using transform %O. Got fields: %O", [| box tfs; box fields |])
  let dataTyp, isSeries = 
    match tfs with 
    | (GetSeries _)::_ -> 
        match fields with
        | [kf; vf] ->  
            ctx.LookupNamed "series" (*[Type.Primitive kf.Type; Type.Primitive vf.Type]*), true
        | _ -> failwith "makeDataMember: Series should have key and value"
    | _ -> 
        let membs = 
          fields |> Array.ofSeq |> Array.map (fun fld ->
            let memTy, memConv = getTypeAndEmitter fld.Type
            let emitter = { Emit = fun (inst, _) -> memConv <| (inst /?/ str fld.Name) }
            Member.Property(fld.Name, memTy, [docMeta (Documentation.Text "")], emitter))
        let recTyp = makeObjectType membs
        ctx.LookupNamed "series" (*[Type.Primitive PrimitiveType.Number; recTyp ]*), false

  let tfs = if isSeries then tfs else GetTheData::tfs
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
  Member.Property(name, dataTyp, [meta1; meta2], makeDataEmitter isPreview isSeries tfs)

and handleGetSeriesRequest ctx rest k v = 
  match k, v with
  | "!", "!" ->
    [ for field in ctx.Fields ->
        makeProperty ctx ("with key " + field.Name) (GetSeries(field.Name, "!")::rest) ]
    |> makeObjectType
  | k, "!" ->
    [ for field in ctx.Fields ->
        makeDataMember ctx ("and value " + field.Name) false (GetSeries(k, field.Name)::rest) ]
    |> makeObjectType
  | _ -> 
    failwith "handleGetSeriesRequest: Should not happen"
  
and handlePagingRequest ctx rest pgid ops =
  let takeMemb = 
    makeMethod ctx "take" (Empty::Paging(List.rev (Take(pgid + "-take")::ops))::rest) (pgid + "-take") ["count", PrimitiveType.Number] 
  let skipMemb = 
    makeMethod ctx "skip" (Paging(Skip(pgid + "-skip")::ops)::rest) (pgid + "-skip") ["count", PrimitiveType.Number] 
  let thenMemb = 
    makeProperty ctx "then" (Empty::Paging(List.rev ops)::rest)
  ( match ops with
    | [] -> [skipMemb; takeMemb; thenMemb]
    | [Skip _] -> [takeMemb; thenMemb]
    | _ -> failwith "handlePagingRequest: Shold not happen" ) |> makeObjectType

and handleDropRequest ctx rest dropped = 
  let droppedFields = set dropped
  [ yield makeProperty ctx "then" (Empty::DropColumns(dropped)::rest)
    for field in ctx.Fields do
      if not (droppedFields.Contains field.Name) then
        yield 
          makeProperty ctx ("drop " + field.Name) (DropColumns(field.Name::dropped)::rest) ]
  |> makeObjectType    

and handleSortRequest ctx rest keys = 
  let usedKeys = set (List.map fst keys)
  [ yield makeProperty ctx "then" (Empty::SortBy(keys)::rest)
    for field in ctx.Fields do
      if not (usedKeys.Contains field.Name) then
        let doc = sprintf "Use the field '%s' as the next sorting keys" field.Name
        let prefix = if keys = [] then "by " else "and by "
        yield makeProperty ctx (prefix + field.Name) (SortBy((field.Name, Ascending)::keys)::rest) 
        yield makeProperty ctx (prefix + field.Name + " descending") (SortBy((field.Name, Descending)::keys)::rest) ]
  |> makeObjectType    


and aggregationMembers ctx rest keys aggs = 
  let containsCountAll = aggs |> Seq.exists ((=) CountAll)
  let containsField fld = aggs |> Seq.exists (function 
    | CountDistinct f | ReturnUnique f | ConcatValues f | Sum f | Mean f -> f = fld | CountAll | GroupKey -> false)
  let makeAggMember name agg = 
    makeProperty ctx name (GroupBy(keys,agg::aggs)::rest) 

  [ yield makeProperty ctx "then" (Empty::GroupBy(keys, aggs)::rest) 
    if not containsCountAll then 
      yield makeAggMember "count all" CountAll
    for fld in ctx.Fields do
      if not (containsField fld.Name) then
        yield makeAggMember ("count distinct " + fld.Name) (CountDistinct fld.Name) 
        yield makeAggMember ("return unique " + fld.Name) (ReturnUnique fld.Name) 
        if isConcatenable fld.Type then
          yield makeAggMember ("concatenate values of " + fld.Name) (ConcatValues fld.Name)
        if isNumeric fld.Type then
          yield makeAggMember ("average " + fld.Name) (Mean fld.Name)
          yield makeAggMember ("sum " + fld.Name) (Sum fld.Name) ]

and handleGroupAggRequest ctx rest keys aggs =
  aggregationMembers ctx rest keys aggs  
  |> makeObjectType  
  
and handleGroupRequest ctx rest keys = 
  let prefix = if List.isEmpty keys then "by " else "and "
  [ for field in ctx.Fields ->
      makeProperty ctx (prefix + field.Name) (GroupBy(field.Name::keys, [])::rest) 
    if not (List.isEmpty keys) then
      yield! aggregationMembers ctx rest keys [GroupKey] ]
  |> makeObjectType  

and handleFilterEqNeqRequest ctx rest (fld, eq) conds = async {
  let tfs = if List.isEmpty conds then rest else FilterBy(conds)::rest
  let url = ctx.Root + "?" + (GetRange(fld)::tfs |> List.rev |> Transform.toUrl)
  let! options = Http.Request("GET", url)
  let options = jsonParse<string[]> options
  return
    [ for opt in options do
        yield makeProperty ctx opt (FilterBy((fld, eq, opt)::conds)::rest) ] 
    |> makeObjectType }

and handleFilterRequest ctx rest conds = 
  let prefix = if List.isEmpty conds then "" else "and "
  [ for field in ctx.Fields do
      yield makeProperty ctx (prefix + field.Name + " is") (FilterBy((field.Name, true, "!")::conds)::rest) 
      yield makeProperty ctx (prefix + field.Name + " is not") (FilterBy((field.Name, false, "!")::conds)::rest) 
      //|> withDocs (sprintf "Group by %s" (field.Name.ToLower()))
      //    ( "Creates groups based on the value of " + field.Name + " and calculte summary " +
      //      "values for each group. You can specify a number of summary calculations in the " + 
      //      "following list:")
      //|> withCreateAction "Aggregation operations" 
    if not (List.isEmpty conds) then
      yield makeProperty ctx "then" (Empty::FilterBy(conds)::rest) ]
  |> makeObjectType  

and makePivotTypeImmediate ctx tfs = async {
  let last, rest = match tfs with last::rest -> last, rest | _ -> Empty, []
  let ctx = { ctx with Fields = Transform.transformFields ctx.InputFields (List.rev rest) }
  match last with
  // Starting a new pivoting operation
  | Empty ->
    return
      [ makeProperty ctx "group data" (GroupBy([], [])::rest) 
        makeProperty ctx "filter data" (FilterBy([])::rest) 
        makeProperty ctx "sort data" (SortBy([])::rest) 
        makeProperty ctx "drop columns" (DropColumns([])::rest) 
        makeProperty ctx "paging" (Paging([])::rest) 
        makeProperty ctx "get series" (GetSeries("!","!")::rest) 
        makeDataMember ctx "get the data" false rest ]
      |> makeObjectType    
  // 
  | GetSeries(k, v) ->
      return handleGetSeriesRequest ctx rest k v
  | Paging(ops) ->
      let pgid = rest |> Seq.sumBy (function Paging _ -> 1 | _ -> 0) |> sprintf "pgid-%d"  
      return handlePagingRequest ctx rest pgid ops
  | SortBy(keys) ->
      return handleSortRequest ctx rest keys
  | DropColumns(dropped) ->
      return handleDropRequest ctx rest dropped
  | FilterBy((fld, eq, "!")::conds) ->
      return! handleFilterEqNeqRequest ctx rest (fld, eq) conds
  | FilterBy(conds) ->
      return handleFilterRequest ctx rest conds
  | GroupBy(flds, []) ->
      return handleGroupRequest ctx rest flds
  | GroupBy(flds, aggs) ->
      return handleGroupAggRequest ctx rest flds aggs 
  | GetTheData | GetRange _ | Metadata ->
      return failwith "makePivotTypeImmediate: GetTheData, GetRange and Metadata shouldn't be of pivot type" }

and adjustForPreview tfs = 
  match tfs with
  | GroupBy([], _)::tfs -> tfs // We do not yet know the grouping key, so return original data
  | GroupBy(k, [])::tfs -> GroupBy(k, [GroupKey])::tfs // We do not have any aggregations yet
  | GetSeries _::tfs -> tfs // We do not yet know the key/value of the series, so return original data
  | _ -> tfs

and withPreview ctx tfs typ = 
  match typ with
  | Type.Object(o) -> 
      let preview = makeDataMember ctx "preview" true (adjustForPreview tfs)
      makeObjectType (Array.append [| preview |] o.Members) 
  | typ -> failwith "withPreview: Expected object type"

and makePivotType ctx tfs = 
  let guid = Transform.toUrl tfs
  let typ = async {
    try
      let! typ = makePivotTypeImmediate ctx tfs
      return withPreview ctx tfs typ 
    with e ->
      Log.exn("providers", "Failed when generating type for %O with exception %O", tfs, e)      
      return raise e }
  Type.Delayed(Async.CreateNamedFuture guid typ)
  
let providePivotType root name lookupNamed fields =
  let fields = [ for f, t in fields -> { Name = f; Type = t }]
  let typ = makePivotType { Fields = fields; InputFields = fields; LookupNamed = lookupNamed; Root = root } []
  let ctx = ident("PivotContext")
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box []  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box fields  }
  ProvidedType.GlobalValue
    ( name, [meta1; meta2],
      NewExpression(ctx, [str root; ArrayExpression([], None)], None), typ)
