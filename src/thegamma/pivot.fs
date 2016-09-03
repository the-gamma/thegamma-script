module TheGamma.TypeProviders.Pivot

open TheGamma
open TheGamma.Babel
open Fable.Import
open Fable.Core
open Fable.Extensions
open TheGamma.TypePoviders

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
  | GetSeries of string * string
  | Empty

type Field = 
  { Name : string 
    Type : string }

module Transform = 

  let private formatAgg = function
    | GroupKey -> ["key"]
    | CountAll -> ["count-all"]
    | CountDistinct(f) -> ["count-dist"; f]
    | ReturnUnique(f) -> ["unique"; f]
    | ConcatValues(f) -> ["concat-vals"; f]
    | Sum(f) -> ["sum"; f]
    | Mean(f) -> ["mean"; f]

  let toUrl transforms = 
    [ for t in List.rev transforms ->
        match t with
        | FilterBy(conds) -> "filter"::(List.collect (fun (f,b,v) -> [f; (if b then "eq" else "neq"); v]) conds)
        | DropColumns(columns) -> "drop"::columns
        | SortBy(columns) -> "sort"::(List.collect (fun (c, o) -> [c; (if o = Ascending then "asc" else "desc")]) columns)
        | GroupBy(flds, aggs) -> "group"::((List.map (fun f -> "by-" + f) flds) @ (List.collect formatAgg aggs))
        | Paging(ops) -> "page"::(List.collect (function Take k -> ["take"; k] | Skip k -> ["skip"; k]) ops)
        | GetSeries(k, v) -> "series"::k::v::[]
        | Empty -> [] ]
    |> List.mapi (fun i l -> if i = 0 then l else "then"::l)
    |> List.concat
    |> String.concat "/"

  let singleTransformFields fields = function
    | Empty -> fields
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
           | CountAll -> [ { Name = "count"; Type = "num" } ]
           | CountDistinct fld -> [ { Name = oldFields.[fld].Name; Type = "num" } ])
      
  let transformFields fields tfs = 
    tfs |> List.fold singleTransformFields (List.ofSeq fields)

// ------------------------------------------------------------------------------------------------
// Pivot provider
// ------------------------------------------------------------------------------------------------

let trimLeft c (s:string) = s.ToCharArray() |> Array.skipWhile ((=) c) |> System.String
let trimRight c (s:string) = s.ToCharArray() |> Array.rev |> Array.skipWhile ((=) c) |> Array.rev |> System.String

let concatUrl (a:string) (b:string) =
  (trimRight '/' a) + "/" + (trimLeft '/' b)

let withDocs title details membr = 
  match membr with
  | Member.Method(n, tya, args, typ, _, emitter) ->
      Member.Method(n, tya, args, typ, Documentation.Details(title, details), emitter)
  | Member.Property(n, typ, schema, _, emitter) ->
      Member.Property(n, typ, schema, Documentation.Details(title, details), emitter)

let withSchema actTyp fldName listName membr = 
  match membr with
  | Member.Property(n, typ, _, docs, emitter) ->
      let schema = 
        { Type = actTyp
          JSON = 
            [ "@context", box "http://schema.org/"
              "@type", box actTyp 
              fldName, JsInterop.createObj [ "@type", box "ItemList"; "name", box listName ] ]
            |> JsInterop.createObj }
      Member.Property(n, typ, Some schema, docs, emitter)
  | _ -> failwith "withSchema: expected property"

let withCreateAction action membr = 
  membr
let withAddAction action membr = 
  membr
let withThingSchema name annotation membr = 
  membr

let makeObjectType members = 
  { Members = Array.ofSeq members
    Typeargs = [] } |> Type.Object

let isNumeric fld = fld = "num"
let isConcatenable fld = fld = "string"

// From providers.fs
let ident s = IdentifierExpression(s, None)
let str v = StringLiteral(v, None)
let arr l = ArrayExpression(l, None)

let (?) (e:Expression) (s:string) = MemberExpression(e, IdentifierExpression(s, None), false, None)
let (/?/) (e:Expression) a = MemberExpression(e, a, true, None)

let (/@/) (e:Expression) (args) = CallExpression(e, args, None)
let func v f = 
  let body = BlockStatement([ReturnStatement(f (ident v), None)], None)
  FunctionExpression(None, [IdentifierPattern(v, None)], body, false, false, None)

let getTypeAndEmitter = function 
  | "string" -> Type.Primitive("string"), id
  | "num" -> Type.Primitive("num"), fun e -> ident "Number" /@/ [e]
  | _ -> failwith "getTypeAndEmitter: Unknown primitive type"

let propertyEmitter = 
  { Emit = fun (this, _) -> this }

let makeMethodEmitter callid pars =
  { Emit = fun (this, args) -> 
      let args = arr [ for _, v in args -> v ]
      this?addCall /@/ [str callid; args] }

let makeDataEmitter isSeries tfs = 
  { Emit = fun (this, _) -> 
      // TODO: This is not properly recursively transforming values, but they're just int/string, so it's OK
      if isSeries then
        ident("_series")?series?create /@/ 
          [ this?getData /@/ [str (Transform.toUrl (List.rev tfs))]
            str "key"; str "value"; str "" ]
      else
        ident("_series")?series?ordinal /@/ 
          [ this?getData /@/ [str (Transform.toUrl (List.rev tfs))]
            str "key"; str "value"; str "" ] }


// ----------------------------------------------------------------------------
// Transformations
// ----------------------------------------------------------------------------

type Context = 
  { Root : string
    LookupNamed : string -> Type list -> Type
    InputFields : Field list
    Fields : Field list }

let rec makeProperty ctx name tfs = 
  Member.Property(name, makePivotType ctx tfs, None, Documentation.None, propertyEmitter)
  
and makeMethod ctx name tfs callid args = 
  Member.Method
    ( name, [], [ for n, t in args -> n, false, Type.Primitive t ], makePivotType ctx tfs, 
      Documentation.None, makeMethodEmitter callid args )

and makeDataMember ctx name tfs =
  let fields = Transform.transformFields ctx.InputFields (List.rev tfs)
  let dataTyp, isSeries = 
    match tfs with 
    | (GetSeries _)::_ -> 
        match ctx.Fields with
        | [kf; vf] ->  
            ctx.LookupNamed "series" [Type.Primitive kf.Type; Type.Primitive vf.Type], true
        | _ -> failwith "makeDataMember: Series should have key and value"
    | _ -> 
        let membs = 
          fields |> Array.ofSeq |> Array.map (fun fld ->
            let memTy, memConv = getTypeAndEmitter fld.Type
            let emitter = { Emit = fun (inst, _) -> memConv <| (inst /?/ str fld.Name) }
            Member.Property(fld.Name, memTy, None, Documentation.Text "", emitter))
        let recTyp = Type.Object { Members = membs; Typeargs = [] }
        ctx.LookupNamed "series" [Type.Primitive "num"; recTyp ], false
  Member.Property(name, dataTyp, None, Documentation.None, makeDataEmitter isSeries tfs)

and handleGetSeriesRequest ctx rest k v = 
  match k, v with
  | "!", "!" ->
    [ for field in ctx.Fields ->
        makeProperty ctx ("with key " + field.Name) (GetSeries(field.Name, "!")::rest)
        |> withDocs "Get the data" "Here, we select one of the attribute of the data set as the 'key' and one as a 'value'. In the first list, you can choose the key." 
        |> withThingSchema "ListItem" "series key" ]
    |> makeObjectType
  | k, "!" ->
    [ for field in ctx.Fields ->
        makeDataMember ctx ("and value " + field.Name) (GetSeries(k, field.Name)::rest) 
        |> withDocs "Get the data" "In the second list, choose attribute that you want to use as the value."
        |> withThingSchema "ListItem" "series value" ]
    |> makeObjectType
  | _ -> 
    failwith "handleGetSeriesRequest: Should not happen"
  
and handlePagingRequest ctx rest pgid ops =
  let takeMemb = 
    makeMethod ctx "take" (Empty::Paging(List.rev (Take(pgid + "-take")::ops))::rest) (pgid + "-take") ["count", "num"] 
    |> withDocs "" "Take the specified number of rows and drop the rest"
  let skipMemb = 
    makeMethod ctx "skip" (Paging(Skip(pgid + "-skip")::ops)::rest) (pgid + "-skip") ["count", "num"] 
    |> withDocs "" "Skip the specified number of rows and keep the rest"
  let thenMemb = 
    makeProperty ctx "then" (Empty::Paging(List.rev ops)::rest)
    |> withDocs "" "Return the data"
  ( match ops with
    | [] -> [skipMemb; takeMemb]
    | [Skip _] -> [takeMemb; thenMemb]
    | _ -> failwith "handlePagingRequest: Shold not happen" ) |> makeObjectType

and handleDropRequest ctx rest dropped = 
  let droppedFields = set dropped
  [ yield makeProperty ctx "then" (Empty::DropColumns(dropped)::rest) |> withDocs "" "Return the data"
    for field in ctx.Fields do
      if not (droppedFields.Contains field.Name) then
        yield 
          makeProperty ctx ("drop " + field.Name) (DropColumns(field.Name::dropped)::rest) 
          |> withDocs "" (sprintf "Removes the field '%s' from the returned data set" field.Name)
          |> withAddAction "Dropped ctx.Fields" ]
  |> makeObjectType    

and handleSortRequest ctx rest keys = 
  let usedKeys = set (List.map fst keys)
  [ yield makeProperty ctx "then" (Empty::SortBy(keys)::rest) |> withDocs "" "Return the data"
    for field in ctx.Fields do
      if not (usedKeys.Contains field.Name) then
        let doc = sprintf "Use the field '%s' as the next sorting keys" field.Name
        let prefix = if keys = [] then "by " else "and by "
        yield makeProperty ctx (prefix + field.Name) (SortBy((field.Name, Ascending)::keys)::rest) 
              |> withDocs "" doc 
              |> withAddAction "Fields used for sorting"
        yield makeProperty ctx (prefix + field.Name + " descending") (SortBy((field.Name, Descending)::keys)::rest) 
              |> withDocs "" doc 
              |> withAddAction "Fields used for sorting" ]
  |> makeObjectType    


and aggregationMembers ctx rest keys aggs = 
  let containsCountAll = aggs |> Seq.exists ((=) CountAll)
  let containsField fld = aggs |> Seq.exists (function 
    | CountDistinct f | ReturnUnique f | ConcatValues f | Sum f | Mean f -> f = fld | CountAll | GroupKey -> false)
  let makeAggMember name agg doc = 
    makeProperty ctx name (GroupBy(keys,agg::aggs)::rest) |> withDocs "" doc
    |> withAddAction "Aggregation operations"

  [ yield makeProperty ctx "then" (Empty::GroupBy(keys, aggs)::rest) |> withDocs "" "Get data or perform another transformation"
    if not containsCountAll then 
      yield makeAggMember "count all" CountAll "Count the number of items in the group"
    for fld in ctx.Fields do
      if not (containsField fld.Name) then
        yield makeAggMember ("count distinct " + fld.Name) (CountDistinct fld.Name) 
                "Count the number of distinct values of the field"
        yield makeAggMember ("return unique " + fld.Name) (ReturnUnique fld.Name) 
                "Add the value of the field assuming it is unique in the group"
        if isConcatenable fld.Type then
          yield makeAggMember ("concatenate values of " + fld.Name) (ConcatValues fld.Name)
                  "Concatenate all values of the field"
        if isNumeric fld.Type then
          yield makeAggMember ("average " + fld.Name) (Mean fld.Name)
                  "Calculate the average value of the field in the group" 
          yield makeAggMember ("sum " + fld.Name) (Sum fld.Name)
                  "Sum the values of the field in the group" ]

and handleGroupAggRequest ctx rest keys aggs =
  aggregationMembers ctx rest keys aggs  
  |> makeObjectType  
  
and handleGroupRequest ctx rest keys = 
  let prefix = if List.isEmpty keys then "by " else "and "
  [ for field in ctx.Fields ->
      makeProperty ctx (prefix + field.Name) (GroupBy(field.Name::keys, [])::rest) 
      |> withDocs (sprintf "Group by %s" (field.Name.ToLower()))
          ( "Creates groups based on the value of " + field.Name + " and calculte summary " +
            "values for each group. You can specify a number of summary calculations in the " + 
            "following list:")
      |> withCreateAction "Aggregation operations" 
    if not (List.isEmpty keys) then
      yield! aggregationMembers ctx rest keys [GroupKey] ]
  |> makeObjectType  

and handleFilterEqNeqRequest ctx rest (fld, eq) conds = async {
  let url = concatUrl (concatUrl ctx.Root "range") (FilterBy(conds)::rest |> List.rev |> Transform.toUrl)
  let! options = Http.Request("GET", url + "?" + fld)
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
      yield makeProperty ctx "then" (Empty::FilterBy(conds)::rest) |> withDocs "" "Return the data" ]
  |> makeObjectType  

and makePivotTypeImmediate ctx tfs = async {
  let last, rest = match tfs with last::rest -> last, rest | _ -> Empty, []
  let ctx = { ctx with Fields = Transform.transformFields ctx.InputFields (List.rev rest) }
  match last with
  // Starting a new pivoting operation
  | Empty ->
    return
      [ makeProperty ctx "group data" (GroupBy([], [])::rest) 
          |> withDocs "" "Lets you perform pivot table aggregations."
        makeProperty ctx "filter data" (FilterBy([])::rest) 
          |> withDocs "" "Lets you filter data in the table."
        makeProperty ctx "sort data" (SortBy([])::rest) 
          |> withDocs "Sort the data" ("Specify how the data is sorted. You can choose one or more attributes " +
              "to use for sorting in the following list. Choose 'descending' to sort the values from largest value " +
              "to smallest value.")
          |> withCreateAction "Fields used for sorting"
        makeProperty ctx "drop columns" (DropColumns([])::rest) 
          |> withDocs "Filter returned attributes" ("Specify which attributes of the data sets should be returned. " +
              "By default you'll get all available attributes, but you can drop uninteresting attributes by listing " +
              "them in the following list:")
          |> withCreateAction "Dropped ctx.Fields"
        makeProperty ctx "paging" (Paging([])::rest) |> withDocs "" "Take a number of rows or skip a number of rows." 
        makeProperty ctx "get series" (GetSeries("!","!")::rest) |> withDocs "" "Get a single key-value series from the data set." 
        makeDataMember ctx "get the data" rest |> withDocs "" "Returns the transformed data" ]
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
      return handleGroupAggRequest ctx rest flds aggs }

and makePivotType ctx tfs = 
  let guid = Transform.toUrl tfs
  Type.Delayed("pivot: " + guid, Async.AsFuture guid (makePivotTypeImmediate ctx tfs))
  
let providePivotType root name lookupNamed fields =
  let fields = [ for f, t in fields -> { Name = f; Type = t }]
  let typ = makePivotType { Fields = fields; InputFields = fields; LookupNamed = lookupNamed; Root = root } []
  let ctx = ident("_restruntime")?PivotContext
  ProvidedType.GlobalValue
    ( name, 
      NewExpression(ctx, [str (concatUrl root "data"); ArrayExpression([], None)], None), typ)
