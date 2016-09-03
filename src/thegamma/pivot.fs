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

    (*
let nada = { Emit = fun (inst, args) -> Babel.NullLiteral(None) }

let rec seriesTy() = 
  { new Future<_> with
      member x.Then(f) = 
        Type.Object 
          { Members = 
            [ Member.Method("sortValues", ["reverse", Type.Primitive "bool"], seriesTy (), nada)
              Member.Method("take", ["count", Type.Primitive "num"], seriesTy (), nada) ] } |> f } |> Type.Delayed
              *)


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

let rec makeProperty lookupNamed fields name tfs = 
  Member.Property(name, makePivotType lookupNamed fields tfs, None, Documentation.None, propertyEmitter)
  
and makeMethod lookupNamed fields name tfs callid args = 
  Member.Method
    ( name, [], [ for n, t in args -> n, false, Type.Primitive t ], makePivotType lookupNamed fields tfs, 
      Documentation.None, makeMethodEmitter callid args )

and makeDataMember (lookupNamed:string -> Type list -> Type) inputFields name tfs =
  let fields = Transform.transformFields inputFields (List.rev tfs)
  let dataTyp, isSeries = 
    match tfs with 
    | (GetSeries _)::_ -> 
        match fields with
        | [kf; vf] ->  
            lookupNamed "series" [Type.Primitive kf.Type; Type.Primitive vf.Type], true
        | _ -> failwith "makeDataMember: Series should have key and value"
    | _ -> 
        let membs = 
          fields |> Array.ofSeq |> Array.map (fun fld ->
            let memTy, memConv = getTypeAndEmitter fld.Type
            let emitter = { Emit = fun (inst, _) -> memConv <| (inst /?/ str fld.Name) }
            Member.Property(fld.Name, memTy, None, Documentation.Text "", emitter))
        let recTyp = Type.Object { Members = membs; Typeargs = [] }
        lookupNamed "series" [Type.Primitive "num"; recTyp ], false
  Member.Property(name, dataTyp, None, Documentation.None, makeDataEmitter isSeries tfs)

and handleGetSeriesRequest lookupNamed inputFields fields rest k v = 
  match k, v with
  | "!", "!" ->
    [ for field in fields ->
        makeProperty lookupNamed fields ("with key " + field.Name) (GetSeries(field.Name, "!")::rest)
        |> withDocs "Get the data" "Here, we select one of the attribute of the data set as the 'key' and one as a 'value'. In the first list, you can choose the key." 
        |> withThingSchema "ListItem" "series key" ]
    |> makeObjectType
  | k, "!" ->
    [ for field in fields ->
        makeDataMember lookupNamed inputFields ("and value " + field.Name) (GetSeries(k, field.Name)::rest) 
        |> withDocs "Get the data" "In the second list, choose attribute that you want to use as the value."
        |> withThingSchema "ListItem" "series value" ]
    |> makeObjectType
  | _ -> 
    failwith "handleGetSeriesRequest: Should not happen"
  
and handlePagingRequest lookupNamed fields rest pgid ops =
  let takeMemb = 
    makeMethod lookupNamed fields "take" (Empty::Paging(List.rev (Take(pgid + "-take")::ops))::rest) (pgid + "-take") ["count", "num"] 
    |> withDocs "" "Take the specified number of rows and drop the rest"
  let skipMemb = 
    makeMethod lookupNamed fields "skip" (Paging(Skip(pgid + "-skip")::ops)::rest) (pgid + "-skip") ["count", "num"] 
    |> withDocs "" "Skip the specified number of rows and keep the rest"
  let thenMemb = 
    makeProperty lookupNamed fields "then" (Empty::Paging(List.rev ops)::rest)
    |> withDocs "" "Return the data"
  ( match ops with
    | [] -> [skipMemb; takeMemb]
    | [Skip _] -> [takeMemb; thenMemb]
    | _ -> failwith "handlePagingRequest: Shold not happen" ) |> makeObjectType

and handleDropRequest lookupNamed fields rest dropped = 
  let droppedFields = set dropped
  [ yield makeProperty lookupNamed fields "then" (Empty::DropColumns(dropped)::rest) |> withDocs "" "Return the data"
    for field in fields do
      if not (droppedFields.Contains field.Name) then
        yield 
          makeProperty lookupNamed fields ("drop " + field.Name) (DropColumns(field.Name::dropped)::rest) 
          |> withDocs "" (sprintf "Removes the field '%s' from the returned data set" field.Name)
          |> withAddAction "Dropped fields" ]
  |> makeObjectType    

and handleSortRequest lookupNamed fields rest keys = 
  let usedKeys = set (List.map fst keys)
  [ yield makeProperty lookupNamed fields "then" (Empty::SortBy(keys)::rest) |> withDocs "" "Return the data"
    for field in fields do
      if not (usedKeys.Contains field.Name) then
        let doc = sprintf "Use the field '%s' as the next sorting keys" field.Name
        let prefix = if keys = [] then "by " else "and by "
        yield makeProperty lookupNamed fields (prefix + field.Name) (SortBy((field.Name, Ascending)::keys)::rest) 
              |> withDocs "" doc 
              |> withAddAction "Fields used for sorting"
        yield makeProperty lookupNamed fields (prefix + field.Name + " descending") (SortBy((field.Name, Descending)::keys)::rest) 
              |> withDocs "" doc 
              |> withAddAction "Fields used for sorting" ]
  |> makeObjectType    


and aggregationMembers lookupNamed fields rest keys aggs = 
  let containsCountAll = aggs |> Seq.exists ((=) CountAll)
  let containsField fld = aggs |> Seq.exists (function 
    | CountDistinct f | ReturnUnique f | ConcatValues f | Sum f | Mean f -> f = fld | CountAll | GroupKey -> false)
  let makeAggMember name agg doc = 
    makeProperty lookupNamed fields name (GroupBy(keys,agg::aggs)::rest) |> withDocs "" doc
    |> withAddAction "Aggregation operations"

  [ yield makeProperty lookupNamed fields "then" (Empty::GroupBy(keys, aggs)::rest) |> withDocs "" "Get data or perform another transformation"
    if not containsCountAll then 
      yield makeAggMember "count all" CountAll "Count the number of items in the group"
    for fld in fields do
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

and handleGroupAggRequest lookupNamed fields rest keys aggs =
  aggregationMembers lookupNamed fields rest keys aggs  
  |> makeObjectType  
  
and handleGroupRequest lookupNamed fields rest keys = 
  let prefix = if List.isEmpty keys then "by " else "and "
  [ for field in fields ->
      makeProperty lookupNamed fields (prefix + field.Name) (GroupBy(field.Name::keys, [])::rest) 
      |> withDocs (sprintf "Group by %s" (field.Name.ToLower()))
          ( "Creates groups based on the value of " + field.Name + " and calculte summary " +
            "values for each group. You can specify a number of summary calculations in the " + 
            "following list:")
      |> withCreateAction "Aggregation operations" 
    if not (List.isEmpty keys) then
      yield! aggregationMembers lookupNamed fields rest keys [GroupKey] ]
  |> makeObjectType  

and makePivotTypeImmediate lookupNamed (inputFields:seq<Field>) tfs = 
  let last, rest = match tfs with last::rest -> last, rest | _ -> Empty, []
  let fields = Transform.transformFields inputFields (List.rev rest)
  match last with
  // Starting a new pivoting operation
  | Empty ->
      [ makeProperty lookupNamed fields "group data" (GroupBy([], [])::rest) |> withDocs "" "Lets you perform pivot table aggregations."
        makeProperty lookupNamed fields "sort data" (SortBy([])::rest) 
          |> withDocs "Sort the data" ("Specify how the data is sorted. You can choose one or more attributes " +
              "to use for sorting in the following list. Choose 'descending' to sort the values from largest value " +
              "to smallest value.")
          |> withCreateAction "Fields used for sorting"
        makeProperty lookupNamed fields "filter columns" (DropColumns([])::rest) 
          |> withDocs "Filter returned attributes" ("Specify which attributes of the data sets should be returned. " +
              "By default you'll get all available attributes, but you can drop uninteresting attributes by listing " +
              "them in the following list:")
          |> withCreateAction "Dropped fields"
        makeProperty lookupNamed fields "paging" (Paging([])::rest) |> withDocs "" "Take a number of rows or skip a number of rows." 
        makeProperty lookupNamed fields "get series" (GetSeries("!","!")::rest) |> withDocs "" "Get a single key-value series from the data set." 
        makeDataMember lookupNamed inputFields "get the data" rest |> withDocs "" "Returns the transformed data" ]
      |> makeObjectType    
  // 
  | GetSeries(k, v) ->
      handleGetSeriesRequest lookupNamed inputFields fields rest k v
  | Paging(ops) ->
      let pgid = rest |> Seq.sumBy (function Paging _ -> 1 | _ -> 0) |> sprintf "pgid-%d"  
      handlePagingRequest lookupNamed fields rest pgid ops
  | SortBy(keys) ->
      handleSortRequest lookupNamed fields rest keys
  | DropColumns(dropped) ->
      handleDropRequest lookupNamed fields rest dropped
  | GroupBy(flds, []) ->
      handleGroupRequest lookupNamed fields rest flds
  | GroupBy(flds, aggs) ->
      handleGroupAggRequest lookupNamed fields rest flds aggs

and makePivotType lookupNamed fields tfs = 
  Type.Delayed("pivot", Async.AsFuture "pivot" <| async {
    return makePivotTypeImmediate lookupNamed fields tfs })
  
let providePivotType root name lookupNamed fields =
  let ctx = ident("_restruntime")?PivotContext
  let typ = makePivotType lookupNamed [ for f, t in fields -> { Name = f; Type = t }] [] 
  ProvidedType.GlobalValue(name, NewExpression(ctx, [str root; ArrayExpression([], None)], None), typ)

(*
  Type.Object
    { Members = 
        [ Member.Property("CO2 emissions (kt)", seriesTy (), nada) ] }
*)