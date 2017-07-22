// ------------------------------------------------------------------------------------------------
// Pivot type provider - for expressing data aggregation queries over a table
// ------------------------------------------------------------------------------------------------
module TheGamma.TypeProviders.Pivot

open Fable.Core
open Fable.Import

open TheGamma
open TheGamma.Babel
open TheGamma.Babel.BabelOperators
open TheGamma.Common
open TheGamma.TypeProviders

// ------------------------------------------------------------------------------------------------
// Operations that we can do on the table
// ------------------------------------------------------------------------------------------------

[<RequireQualifiedAccess>]
type GroupAggregation = 
  | GroupKey
  | CountAll
  | CountDistinct of string
  | ConcatValues of string
  | Sum of string
  | Mean of string

[<RequireQualifiedAccess>]
type WindowAggregation = 
  | Min of string
  | Sum of string
  | Max of string
  | Mean of string
  | FirstKey
  | LastKey
  | MiddleKey

type SortDirection =
  | Ascending
  | Descending 

type Paging =
  | Take of string
  | Skip of string
  
type FilterOperator = 
  | And | Or

type RelationalOperator = 
  | Equals 
  | NotEquals 
  | LessThan
  | GreaterThan 
  | InRange
  | Like

type FilterCondition = RelationalOperator * string * string

type Transformation = 
  | DropColumns of string list
  | SortBy of (string * SortDirection) list
  | GroupBy of string list * GroupAggregation list
  | WindowBy of string * string * WindowAggregation list
  | ExpandBy of string * WindowAggregation list
  | FilterBy of FilterOperator * FilterCondition list
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

  let private formatGroupAgg = function
    | GroupAggregation.GroupKey -> "key"
    | GroupAggregation.CountAll -> "count-all"
    | GroupAggregation.CountDistinct(f) -> "count-dist " + Ast.escapeIdent f
    | GroupAggregation.ConcatValues(f) -> "concat-vals " + Ast.escapeIdent f
    | GroupAggregation.Sum(f) -> "sum " + Ast.escapeIdent f
    | GroupAggregation.Mean(f) -> "mean " + Ast.escapeIdent f

  let private formatWinAgg = function
    | WindowAggregation.FirstKey -> "first-key"
    | WindowAggregation.MiddleKey -> "mid-key"
    | WindowAggregation.LastKey -> "last-key"
    | WindowAggregation.Mean(f) -> "mean " + Ast.escapeIdent f
    | WindowAggregation.Min(f) -> "min " + Ast.escapeIdent f
    | WindowAggregation.Max(f) -> "max " + Ast.escapeIdent f
    | WindowAggregation.Sum(f) -> "sum " + Ast.escapeIdent f

  let formatCondition (op, f, v) =
    let op = 
      match op with Equals -> "eq" | NotEquals -> "neq" | LessThan -> "lte" | GreaterThan -> "gte" | InRange -> "in" | Like -> "like"
    Ast.escapeIdent f + " " + op + " " + Ast.escapeIdent v

  let toUrl transforms = 
    [ for t in transforms ->
        match t with
        | GetTheData -> []
        | Metadata -> ["metadata", []]
        | GetRange fld -> ["range", [fld]]
        | FilterBy(op, conds) -> ["filter", (match op with And -> "and" | Or -> "or")::(List.map formatCondition conds)]
        | DropColumns(columns) -> ["drop", columns]
        | SortBy(columns) -> ["sort", (List.map (fun (c, o) -> c + (if o = Ascending then " asc" else " desc")) columns)]
        | GroupBy(flds, aggs) -> ["groupby", (List.map (fun fld -> "by " + Ast.escapeIdent fld) flds) @ (List.map formatGroupAgg aggs)]
        | WindowBy(fld, size, aggs) -> ["windowby", ("by " + Ast.escapeIdent fld) :: size :: (List.map formatWinAgg aggs)]
        | ExpandBy(fld, aggs) -> ["expandby", ("by " + Ast.escapeIdent fld) :: (List.map formatWinAgg aggs)]
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
    | ExpandBy(key, aggs) 
    | WindowBy(key, _, aggs) ->
        let oldFields = dict [ for f in fields -> f.Name, f ]
        aggs 
        |> List.collect (function
           | WindowAggregation.FirstKey -> [ { Name = "first " + key; Type = oldFields.[key].Type } ]
           | WindowAggregation.MiddleKey -> [ { Name = "middle " + key; Type = oldFields.[key].Type } ]
           | WindowAggregation.LastKey -> [ { Name = "last " + key; Type = oldFields.[key].Type } ]
           | WindowAggregation.Mean fld 
           | WindowAggregation.Min fld 
           | WindowAggregation.Sum fld 
           | WindowAggregation.Max fld -> [ oldFields.[fld] ])
    | GroupBy(flds, aggs) ->
        let oldFields = dict [ for f in fields -> f.Name, f ]
        aggs 
        |> List.collect (function
           | GroupAggregation.GroupKey -> List.map (fun f -> oldFields.[f]) flds
           | GroupAggregation.ConcatValues fld
           | GroupAggregation.Sum fld -> [ oldFields.[fld] ]
           | GroupAggregation.Mean fld -> [ oldFields.[fld] ]
           | GroupAggregation.CountAll -> [ { Name = "count"; Type = PrimitiveType.Number } ]
           | GroupAggregation.CountDistinct fld -> [ { Name = oldFields.[fld].Name; Type = PrimitiveType.Number } ])
      
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

type PivotObject(members:seq<Member>) =
  member x.MemberNames = [ for m in members -> m.Name ]
  interface ObjectType with 
    member x.Members = Array.ofSeq members 
    member x.TypeEquals y = 
      match y with
      | :? PivotObject as y -> y.MemberNames = x.MemberNames 
      | _ -> false

let makeObjectType members = Type.Object(PivotObject(members))

let isNumeric fld = fld = PrimitiveType.Number
let isBool fld = fld = PrimitiveType.Bool
let isDate fld = fld = PrimitiveType.Date
let isConcatenable fld = fld = PrimitiveType.String

let getTypeAndEmitter = function 
  | PrimitiveType.String -> Type.Primitive(PrimitiveType.String), id
  | PrimitiveType.Date -> Type.Primitive(PrimitiveType.String), fun e -> NewExpression(ident("Date"), [ident("Date")?parse /@/ [e]], None)
  | PrimitiveType.Number -> Type.Primitive(PrimitiveType.Number), fun e -> ident "Number" /@/ [e]
  | PrimitiveType.Bool -> Type.Primitive(PrimitiveType.Number), fun e -> ident "Boolean" /@/ [e]
  | PrimitiveType.Unit -> Type.Primitive(PrimitiveType.Unit), fun e -> NullLiteral(None)

let propertyEmitter = 
  { Emit = fun this -> this }

let makeMethodEmitter callid pars =
  { Emit = fun this -> funcN (Seq.length pars) (fun args ->
      let args = arr [ for v in args -> v ]
      this?addCall /@/ [str callid; args]) }

let makeDataEmitter isPreview isSeries convValues tfs = 
  { Emit = fun this -> 
      if isSeries then
        ident("series")?create /@/ 
          [ this?getData /@/ [convValues; str (Transform.toUrl (List.rev tfs)); bool isPreview]
            str "key"; str "value"; str "" ]
      else
        ident("series")?ordinal /@/ 
          [ this?getData /@/ [convValues; str (Transform.toUrl (List.rev tfs)); bool isPreview]
            str "key"; str "value"; str "" ] }


// ------------------------------------------------------------------------------------------------
// Transformations
// ------------------------------------------------------------------------------------------------

type Context = 
  { Root : string
    IgnoreFiltersInRange : bool
    LookupNamed : string -> Type
    InputFields : Field list
    Fields : Field list }

let rec makeProperty ctx name tfs = 
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
  { Member.Name = name; Type = makePivotType ctx tfs; Metadata = [meta1; meta2]; Emitter = propertyEmitter }
  
and makeMethod ctx name tfs callid args = 
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
  { Member.Name = name; Metadata = [meta1; meta2]
    Type = 
      Type.Method
        ( [ for n, t in args -> { MethodArgument.Name = n; Optional = false; Static = false; Type = Type.Primitive t } ],
          (fun ts -> 
              let ts = List.map fst ts
              if Types.listsEqual ts args (fun t1 (_, t2) -> Types.typesEqual t1 (Type.Primitive t2)) 
                then Some(makePivotType ctx tfs) else None) )
    Emitter = makeMethodEmitter callid args }

and makeDataMember ctx name isPreview tfs =
  let fields = Transform.transformFields ctx.InputFields (List.rev tfs)
  Log.trace("providers", "Make data member using transform %O. Got fields: %O", [| box tfs; box fields |])
  let isSeries, dataTyp, convValues = 
    match tfs with 
    | (GetSeries _)::_ -> 
        match fields with
        | [kf; vf] ->  
            true,
            FSharpProvider.applyTypes (ctx.LookupNamed "series") [Type.Primitive kf.Type; Type.Primitive vf.Type], 
            func "o" (fun arg -> 
              arr [ snd (getTypeAndEmitter kf.Type) (arg /?/ num 0.)
                    snd (getTypeAndEmitter vf.Type) (arg /?/ num 1.) ])
        | _ -> failwith "makeDataMember: Series should have key and value"
    | _ -> 
        let convs, membs = 
          fields 
            |> Array.ofSeq 
            |> Array.map (fun fld ->
              let memTy, memConv = getTypeAndEmitter fld.Type
              let emitter = { Emit = fun inst -> inst /?/ str fld.Name }
              (fld.Name, memConv),
              { Member.Name = fld.Name; Type = memTy; Emitter = emitter;
                Metadata = [docMeta (Documentation.Text "")] })
            |> Array.unzip
        let recTyp = makeObjectType membs
        false,
        FSharpProvider.applyTypes (ctx.LookupNamed "series") [Type.Primitive PrimitiveType.Number; recTyp ],
        func "o" (fun arg ->
          let mems = 
            [ for fld, conv in convs ->
                ObjectProperty(str fld, conv (arg /?/ str fld), true, None) ]
          ObjectExpression(mems, None) )

  let tfs = if isSeries then tfs else GetTheData::tfs
  let meta = 
    [ yield { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box tfs }
      yield { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box ctx.Fields  }
      if isPreview then
        yield { Context = "http://schema.thegamma.net"; Type = "CompletionItem"; Data = JsInterop.createObj ["hidden", box true] }]
  { Member.Name = name; Type = dataTyp; Metadata = meta; Emitter = makeDataEmitter isPreview isSeries convValues tfs }

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

and handleWindowRequest ctx rest wndid = 
  [ for field in ctx.Fields do 
      if isDate field.Type || isNumeric field.Type then
        yield makeMethod ctx ("window by " + field.Name) (WindowBy(field.Name, wndid, [])::rest) wndid ["size", PrimitiveType.Number] 
        yield makeProperty ctx ("expanding by " + field.Name) (ExpandBy(field.Name, [WindowAggregation.LastKey])::rest) ]
  |> makeObjectType  

and handleWindowExpandAggRequest ctx rest fld make aggs = 
  let containsKey = aggs |> Seq.exists(function
    | WindowAggregation.FirstKey | WindowAggregation.LastKey | WindowAggregation.MiddleKey -> true
    | _ -> false)
  let containsField fld = aggs |> Seq.exists (function 
    | WindowAggregation.Sum f | WindowAggregation.Max f | WindowAggregation.Min f | WindowAggregation.Mean f -> f = fld 
    | WindowAggregation.FirstKey | WindowAggregation.LastKey | WindowAggregation.MiddleKey -> false)

  let makeAggMember name agg = 
    makeProperty ctx name (make(aggs @ [agg])::rest) 

  [ if not (List.isEmpty aggs) then
      yield makeProperty ctx "then" (Empty::make(aggs)::rest) 
    if not containsKey then
      yield makeAggMember ("first " + fld) WindowAggregation.FirstKey
      yield makeAggMember ("last " + fld) WindowAggregation.LastKey
      yield makeAggMember ("middle " + fld) WindowAggregation.MiddleKey
    for fld in ctx.Fields do
      if not (containsField fld.Name) then
        if isNumeric fld.Type || isBool fld.Type then
          yield makeAggMember ("min " + fld.Name) (WindowAggregation.Min fld.Name)
          yield makeAggMember ("sum " + fld.Name) (WindowAggregation.Sum fld.Name)
          yield makeAggMember ("max " + fld.Name) (WindowAggregation.Max fld.Name)
          yield makeAggMember ("mean " + fld.Name) (WindowAggregation.Mean fld.Name) ]
  |> makeObjectType  

and aggregationMembers ctx rest keys aggs = 
  let containsCountAll = aggs |> Seq.exists ((=) GroupAggregation.CountAll)
  let containsField fld = aggs |> Seq.exists (function 
    | GroupAggregation.CountDistinct f | GroupAggregation.ConcatValues f 
    | GroupAggregation.Sum f | GroupAggregation.Mean f -> f = fld 
    | GroupAggregation.CountAll | GroupAggregation.GroupKey -> false)

  let makeAggMember name agg = 
    makeProperty ctx name (GroupBy(keys,aggs @ [agg])::rest) 

  [ yield makeProperty ctx "then" (Empty::GroupBy(keys, aggs)::rest) 
    if not containsCountAll then 
      yield makeAggMember "count all" GroupAggregation.CountAll
    for fld in ctx.Fields do
      if not (containsField fld.Name) then
        yield makeAggMember ("count distinct " + fld.Name) (GroupAggregation.CountDistinct fld.Name) 
        if isConcatenable fld.Type then
          yield makeAggMember ("concatenate values of " + fld.Name) (GroupAggregation.ConcatValues fld.Name)
        if isNumeric fld.Type || isBool fld.Type then
          yield makeAggMember ("average " + fld.Name) (GroupAggregation.Mean fld.Name)
          yield makeAggMember ("sum " + fld.Name) (GroupAggregation.Sum fld.Name) ]

and handleGroupAggRequest ctx rest keys aggs =
  aggregationMembers ctx rest keys aggs  
  |> makeObjectType  
  
and handleGroupRequest ctx rest keys = 
  let prefix = if List.isEmpty keys then "by " else "and "
  [ for field in ctx.Fields ->
      makeProperty ctx (prefix + field.Name) (GroupBy(field.Name::keys, [])::rest) 
    if not (List.isEmpty keys) then
      yield! aggregationMembers ctx rest keys [GroupAggregation.GroupKey] ]
  |> makeObjectType  

and handleFilterEqNeqRequest ctx rest (fld, eq) op conds = async {
  let tfs = 
    if op = Or then rest 
    elif List.isEmpty conds then rest 
    else FilterBy(op, conds)::rest
  let tfs = 
    tfs |> List.filter (function 
      | FilterBy _ when ctx.IgnoreFiltersInRange -> false 
      | FilterBy(_, conds) when conds |> List.exists (function ((Equals | NotEquals), _, _) -> false | _ -> true) -> false
      | _ -> true)
  let url = ctx.Root + "?" + (GetRange(fld)::tfs |> List.rev |> Transform.toUrl |> Fable.Import.JS.encodeURIComponent)
  let! options = Http.Request("GET", url)
  let options = jsonParse<string[]> options
  return
    [ for opt in options do
        yield makeProperty ctx opt (FilterBy(op, (eq, fld, opt)::conds)::rest) ] 
    |> makeObjectType }

and handleFilterRequest ctx rest flid op conds = 
  let prefixes = 
    match conds, op with
    | [], _ -> ["", And] 
    | _::[], _ -> ["and ", And; "or ", Or]
    | _, And -> ["and ", And] 
    | _, Or -> ["or ", Or]
  [ for prefix, op in prefixes do
      for field in ctx.Fields do
        if field.Type = PrimitiveType.String then
          yield makeProperty ctx (prefix + field.Name + " is") (FilterBy(op, (Equals, field.Name, "!")::conds)::rest) 
          yield makeProperty ctx (prefix + field.Name + " is not") (FilterBy(op, (NotEquals, field.Name, "!")::conds)::rest) 
          yield makeMethod ctx (prefix + field.Name + " contains") (FilterBy(op, (Like, field.Name, flid)::conds)::rest) flid ["text", PrimitiveType.String]
        if field.Type = PrimitiveType.Number then
          yield makeMethod ctx (prefix + field.Name + " is less than") (FilterBy(op, (LessThan, field.Name, flid)::conds)::rest) flid ["value", PrimitiveType.Number]
          yield makeMethod ctx (prefix + field.Name + " is greater than") (FilterBy(op, (GreaterThan, field.Name, flid)::conds)::rest) flid ["value", PrimitiveType.Number]
          yield makeMethod ctx (prefix + field.Name + " is in range") (FilterBy(op, (InRange, field.Name, flid)::conds)::rest) flid ["minimum", PrimitiveType.Number; "maximum", PrimitiveType.Number]
        if field.Type = PrimitiveType.Date then
          yield makeMethod ctx (prefix + field.Name + " is less than") (FilterBy(op, (LessThan, field.Name, flid)::conds)::rest) flid ["value", PrimitiveType.Date]
          yield makeMethod ctx (prefix + field.Name + " is greater than") (FilterBy(op, (GreaterThan, field.Name, flid)::conds)::rest) flid ["value", PrimitiveType.Date]
          yield makeMethod ctx (prefix + field.Name + " is in range") (FilterBy(op, (InRange, field.Name, flid)::conds)::rest) flid ["minimum", PrimitiveType.Date; "maximum", PrimitiveType.Date]
    if not (List.isEmpty conds) then
      yield makeProperty ctx "then" (Empty::FilterBy(op, conds)::rest) ]
  |> makeObjectType  

and makePivotTypeImmediate ctx tfs = async {
  let last, rest = match tfs with last::rest -> last, rest | _ -> Empty, []
  let ctx = { ctx with Fields = Transform.transformFields ctx.InputFields (List.rev rest) }
  match last with
  // Starting a new pivoting operation
  | Empty ->
    return
      [ yield makeProperty ctx "group data" (GroupBy([], [])::rest) 
        yield makeProperty ctx "filter data" (FilterBy(And, [])::rest) 
        yield makeProperty ctx "sort data" (SortBy([])::rest) 
        yield makeProperty ctx "drop columns" (DropColumns([])::rest) 
        yield makeProperty ctx "paging" (Paging([])::rest) 
        yield makeProperty ctx "get series" (GetSeries("!","!")::rest) 
        yield makeDataMember ctx "get the data" false rest 
        if ctx.Fields |> List.exists (fun fld -> fld.Type = PrimitiveType.Date || fld.Type = PrimitiveType.Number) then
          yield makeProperty ctx "windowing" (WindowBy("!", "!", [])::rest) ]
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
  | FilterBy(fop, (rop & (Equals | NotEquals), fld, "!")::conds) ->
      return! handleFilterEqNeqRequest ctx rest (fld, rop) fop conds
  | FilterBy(op, conds) ->
      let flid = conds.Length + (Seq.sumBy (function FilterBy(_, cds) -> cds.Length | _ -> 0) rest)
      return handleFilterRequest ctx rest (sprintf "flid-%d" flid) op conds
  | WindowBy("!", "!", []) ->
      let wnid = rest |> Seq.sumBy (function WindowBy _ -> 1 | _ -> 0) |> sprintf "wnid-%d"
      return handleWindowRequest ctx rest wnid
  | WindowBy(fld, size, aggs) ->
      return handleWindowExpandAggRequest ctx rest fld (fun aggs -> WindowBy(fld, size, aggs)) aggs
  | ExpandBy(fld, aggs) ->
      return handleWindowExpandAggRequest ctx rest fld (fun aggs -> ExpandBy(fld, aggs)) aggs
  | GroupBy(flds, []) ->
      return handleGroupRequest ctx rest flds
  | GroupBy(flds, aggs) ->
      return handleGroupAggRequest ctx rest flds aggs 
  | GetTheData | GetRange _ | Metadata ->
      return failwith "makePivotTypeImmediate: GetTheData, GetRange and Metadata shouldn't be of pivot type" }

and adjustForPreview tfs = 
  match tfs with
  | WindowBy(_,"!",_)::tfs -> tfs // We do not yet know the size 
  | GroupBy([], _)::tfs -> tfs // We do not yet know the grouping key, so return original data
  | GroupBy(k, [])::tfs -> GroupBy(k, [GroupAggregation.GroupKey])::tfs // We do not have any aggregations yet
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
  
let makePivotExpression root = 
  NewExpression(ident("PivotContext"), [str root; ArrayExpression([], None)], None)

let makePivotGlobalValue root name lookupNamed ignoreFilter fields =
  let fields = [ for f, t in fields -> { Name = f; Type = t }]
  let typ = makePivotType { Fields = fields; InputFields = fields; LookupNamed = lookupNamed; Root = root; IgnoreFiltersInRange = ignoreFilter } []
  let meta1 = { Context = "http://schema.thegamma.net/pivot"; Type = "Transformations"; Data = box []  }
  let meta2 = { Context = "http://schema.thegamma.net/pivot"; Type = "Fields"; Data = box fields  }
  ProvidedType.GlobalValue( name, [meta1; meta2], makePivotExpression root, typ)

let providePivotType root ignoreFilter name lookupNamed = async {
  let! membersJson = Http.Request("GET", root + "?metadata")
  let fields = JsHelpers.properties(jsonParse<obj> membersJson) |> Array.map (fun kv -> 
    let typ = 
      match unbox kv.value with
      | "string" -> PrimitiveType.String
      | "bool" -> PrimitiveType.Bool
      | "number" -> PrimitiveType.Number
      | "date" -> PrimitiveType.Date
      | s -> failwith (sprintf "The property '%s' has invalid type '%s'. Only 'string', 'number' and 'bool' are supported." kv.key s)
    kv.key, typ)
  return makePivotGlobalValue root name lookupNamed ignoreFilter fields }