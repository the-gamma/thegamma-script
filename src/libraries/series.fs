namespace TheGamma.Series

open Fable.Core
open TheGamma.Common

// --------------------------------------------------------------------------------------------------------------------
// Series helpers - various JavaScript functions needed for simple series implementation
// --------------------------------------------------------------------------------------------------------------------

module SeriesInternals =
  open System.Collections.Generic

  [<Emit("($0==null)")>]
  let isNull(o:obj) : bool = failwith "never"

  [<Emit("$0[$1]")>]
  let getProperty<'T> (obj:obj) (name:string) : 'T = failwith "never"

  [<Emit("($0 < $1 ? -1 : ($0 == $1 ? 0 : 1))")>]
  let compare (x:'a) (y:'a) : int = failwith "never"

  let slice lo hi (arr:'T[]) =
    Array.init (hi - lo + 1) (fun i -> arr.[lo + i])

  let dictAny (v:seq<'k*'v>) = unbox<IDictionary<'k,'v>> (dict (unbox<seq<obj * obj>> v))

  let zipUnsorted (arr1:_[]) (arr2:_[]) =
    let d1 = dictAny arr1
    let d2 = dictAny arr2
    let res = ResizeArray<_>()
    for kv1 in d1 do
      let v2 =
        if d2.ContainsKey(kv1.Key) then Some(d2.[kv1.Key])
        else None
      res.Add(kv1.Key, (Some kv1.Value, v2))
    for kv2 in d2 do
      if not (d1.ContainsKey(kv2.Key)) then
        res.Add(kv2.Key, (None, Some kv2.Value))
    Array.ofSeq res

  let isSortedUsing test proj (arr:_[]) =
    let rec loop i =
      if i = arr.Length then true
      else test (proj arr.[i-1]) (proj arr.[i]) && loop (i+1)
    arr.Length = 0 || loop 1

  let zipSorted (arr1:('k*'v1)[]) (arr2:('k*'v2)[]) =
    let mutable i1 = 0
    let mutable i2 = 0
    let inline (<.) (a:'k) (b:'k) = compare a b < 0
    let inline eq (a:'k) (b:'k) = compare a b = 0
    let res = ResizeArray<_>()
    while i1 < arr1.Length && i2 < arr2.Length do
      let (k1, v1), (k2, v2) = arr1.[i1], arr2.[i2]
      if eq k1 k2 then
        res.Add(k1, (Some v1, Some v2))
        i1 <- i1 + 1
        i2 <- i2 + 1
      elif k1 <. k2 then
        res.Add(k1, (Some v1, None))
        i1 <- i1 + 1
      elif k2 <. k1 then
        res.Add(k2, (None, Some v2))
        i2 <- i2 + 1
    while i1 < arr1.Length do
      let k1, v1 = arr1.[i1]
      res.Add(k1, (Some v1, None))
      i1 <- i1 + 1
    while i2 < arr2.Length do
      let k2, v2 = arr2.[i2]
      res.Add(k2, (None, Some v2))
      i2 <- i2 + 2
    Array.ofSeq res

  let zipAny (arr1:('k*'v1)[]) (arr2:('k*'v2)[]) =
    let (<=.) (a:'k) (b:'k) = compare a b <= 0
    let (>=.) (a:'k) (b:'k) = compare a b >= 0
    if isSortedUsing (<=.) fst arr1 && isSortedUsing (<=.) fst arr2 then zipSorted arr1 arr2
    elif isSortedUsing (>=.) fst arr1 && isSortedUsing (>=.) fst arr2 then Array.rev (zipSorted (Array.rev arr1) (Array.rev arr2))
    else zipUnsorted arr1 arr2

// --------------------------------------------------------------------------------------------------------------------
// Async series library for TheGamma - implements type `series<'k, 'v>` with various operations
// --------------------------------------------------------------------------------------------------------------------

open SeriesInternals

type ``val``<'k> = 
  { value : Async<'k> }
  member s.map(f) =
    { value = async { 
        let! r = s.value 
        return f r } }

type internal helpers = 
  static member inline lift (f:('a*'b)[] -> ('c*'d)[]) (s:series<_, _>) =
    let nd = async {
      let! vs = s.data |> Async.AwaitFuture
      return f vs } |> Async.StartAsFuture
    { data = nd
      keyName = s.keyName
      valueName = s.valueName
      seriesName = s.seriesName } 
  static member inline asyncLift (f:('a*'b)[] -> Async<('c*'d)[]>) (s:series<_, _>) =
    let nd = async {
      let! vs = s.data |> Async.AwaitFuture
      return! f vs } |> Async.StartAsFuture
    { data = nd
      keyName = s.keyName
      valueName = s.valueName
      seriesName = s.seriesName }

  static member inline liftAggregation f (s:series<_, _>) : ``val``<_> =
    { value = async {
        let! vs = s.data |> Async.AwaitFuture
        return f vs } }

and series<'k, 'v> = 
  internal 
    { data : Future<('k * 'v)[]> 
      keyName : string
      valueName : string
      seriesName : string }

  member internal x.set(data, ?keyName, ?valueName, ?seriesName) = 
    { data = data 
      keyName = defaultArg keyName x.keyName
      valueName = defaultArg valueName x.valueName
      seriesName = defaultArg seriesName x.seriesName }
  member x.setProperties(?keyName, ?valueName, ?seriesName) = 
    { x with 
        keyName = defaultArg keyName x.keyName
        valueName = defaultArg valueName x.valueName
        seriesName = defaultArg seriesName x.seriesName }

//type series =
  static member create(data, keyName, valueName, seriesName) = 
    { data = data |> Async.StartAsFuture; keyName = keyName; valueName = valueName; seriesName = seriesName }

  // TODO: This is where the naming starts to suck
  static member values(values) = 
    let data = async {
      return values |> Array.mapi (fun i v -> i, v) } |> Async.StartAsFuture
    { data = data; keyName = "key"; valueName = "value"; seriesName = "" }

  static member range(from, ``to``) = 
    series<int, int>.values [| from .. ``to`` |]

  static member rangeBy(from, ``to``, step) = 
    series<int, int>.values [| from .. step .. ``to`` |]

  static member ordinal(data, keyName, valueName, seriesName) = 
    let data = async {
      let! values = data
      return values |> Array.mapi (fun i v -> i, v) } |> Async.StartAsFuture
    { data = data; keyName = keyName; valueName = valueName; seriesName = seriesName }

  member s.sortKeys(?reverse) =
    s |> helpers.lift (fun arr ->
      arr |> Array.sortWith (fun (k1, _) (k2, _) -> compare k1 k2)
          |> (if reverse = Some true then Array.rev else id))

  member s.sortValues(?reverse) =
    s |> helpers.lift (fun arr ->
      arr |> Array.sortWith (fun (_,v1) (_,v2) -> compare v1 v2)
          |> (if reverse = Some true then Array.rev else id))

  member s.sortBy(f, ?reverse) =
    s |> helpers.lift (fun arr ->
      arr |> Array.sortWith (fun (_,v1) (_,v2) -> compare (f v1) (f v2))
          |> (if reverse = Some true then Array.rev else id))

  member s.reverse() =
    s |> helpers.lift (Array.rev)

  member s.take(count) =
    s |> helpers.lift (fun arr -> slice 0 ((min arr.Length count)-1) arr)

  member s.skip(count) =
    s |> helpers.lift (fun arr -> slice (min arr.Length count) (arr.Length-1) arr)

  member s.shuffle() =
    s |> helpers.lift (fun arr -> 
      let rnd = System.Random()
      arr |> Array.sortBy (fun _ -> rnd.Next()) )

  member s.map(f) =
    s |> helpers.lift (Array.map (fun (k, v) -> k, f v))
(*
  member s.mapTask(f:'v -> value<'r>) =
    s.set(async {
      let! arr = s.data
      let res = Array.init arr.Length (fun _ -> None)
      for i in 0 .. arr.Length-1 do
        let! r = (f(snd arr.[i])).value
        res.[i] <- Some r
      return Array.init arr.Length (fun i -> fst arr.[i], res.[i].Value)
    })
*)
  member s.mapKeys(f) =
    s |> helpers.lift (Array.map (fun (k, v) -> f k, v))

  member s.mapPairs(f) =
    s |> helpers.lift (Array.map (fun (k, v) -> k, f k v))

  member s.filter(f) =
    s |> helpers.lift (Array.filter (snd >> f))

  member s.filterIndex(f) =
    s |> helpers.lift (Array.mapi (fun i v -> f i, v) >> Array.filter fst >> Array.map snd)

  member s.choose(f) =
    s |> helpers.lift (Array.choose (fun (k, v) -> match f v with None -> None | Some r -> Some(k, r)))

  member s.joinOuter<'v2>(s2:series<'k, 'v2>) : series<'k, 'v option * 'v2 option>=
    let data = async {
      let! v1 = s.data |> Async.AwaitFuture
      let! v2 = s2.data |> Async.AwaitFuture
      return zipAny v1 v2 }
    series<obj,obj>.create(data, s.keyName, "Values", s.seriesName + " and " + s2.seriesName)

  member s.joinInner<'v2>(s2:series<'k, 'v2>) : series<'k, TheGamma.General.pair<'v,'v2>> =
    s.joinOuter(s2).choose(function Some(v1), Some(v2) -> Some(TheGamma.General.pair(v1, v2)) | _ -> None)

  member s.appendScalar(key:'k, value:'v) =
    s |> helpers.lift (fun arr -> Array.append arr [| key, value |])
(*
  member s.appendValue(key:'k, value:value<'v>) =
    s.set(async {
      let! arr = s.data
      let! v = value.value
      return Array.append arr [| key, v |] })
*)
  member s.append(s2:series<'k, 'v>) =
    s.set(async {
      let! arr1 = s.data |> Async.AwaitFuture
      let! arr2 = s2.data |> Async.AwaitFuture
      return Array.append arr1 arr2 } |> Async.StartAsFuture)

  member s.count() =
    s |> helpers.liftAggregation (fun arr -> arr.Length)

  member s.last() =
    s |> helpers.liftAggregation (fun arr -> snd arr.[arr.Length - 1])

  member s.first() =
    s |> helpers.liftAggregation (fun arr -> snd arr.[0])

  member s.sumBy(f:'v -> float) =
    s |> helpers.liftAggregation (Array.sumBy (fun (k, v) -> f v))

  member s.minBy(f) =
    s |> helpers.liftAggregation (Array.minBy (fun (k, v) -> f v))

  member s.maxBy(f) =
    s |> helpers.liftAggregation (Array.maxBy (fun (k, v) -> f v))

  member s.realign(newKeys:series<'k, 'v>, defaultValue) = 
    s |> helpers.asyncLift (fun arr -> async {
      let! newKeys = newKeys.data |> Async.AwaitFuture
      let newKeys = newKeys |> Array.map (fun (k, v) -> unbox<System.IComparable> v)
      let lookup = Map.ofArray (unbox<(System.IComparable * 'v)[]> arr)
      return newKeys |> Array.map (fun k ->
        match lookup.TryFind k with
        | Some res -> unbox<'k> k, res
        | None -> unbox<'k> k, defaultValue) })

  member s.preview() = s.take(10)
      
type ``inline`` =
  { value : ``val``<obj> }
  static member create(value:``val``<'v>) =
    { value = unbox value }
  member i.show(outputId) = Async.StartImmediate <| async {
    let! v = i.value.value
    Fable.Import.Browser.document.getElementById(outputId).innerText <- string v }


(*
open System.Runtime.CompilerServices

[<Extension>]
type SeriesExtensions =
  [<Extension>]
  static member sum(s:series<'k, float>) =
    s |> helpers.liftAggregation (Array.sumBy snd)

  [<Extension>]
  static member series(values:seq<'v>) =
    let getKey i (v:'v) =
      let name = getProperty<string> v "name"
      let id = getProperty<string> v "id"
      if not (isNull name) then name
        elif not (isNull id) then id
          else string i
    let data = async { return values |> Array.ofSeq |> Array.mapi (fun i v -> getKey i v, v) }
    series.create(data, "Key", "Value", "Series")

  [<Extension>]
  static member series(values:list<'v>) =
     SeriesExtensions.series(values :> seq<_>)
[<Extension>]
type ListExtensions =
  [<Extension>]
  static member map(list, f) = List.map f list

[<Extension>]
type TupleExtensions =
  [<Extension>]
  static member map((a,b), f) = (f a, f b)
  [<Extension>]
  static member map((a,b,c), f) = (f a, f b, f c)
  [<Extension>]
  static member map((a,b,c,d), f) = (f a, f b, f c, f d)
  [<Extension>]
  static member map((a,b,c,d,e), f) = (f a, f b, f c, f d, f e)
  [<Extension>]
  static member map((a,b,c,d,e,g), f) = (f a, f b, f c, f d, f e, f g)
  [<Extension>]
  static member map((a,b,c,d,e,g,h), f) = (f a, f b, f c, f d, f e, f g, f h)
  *)