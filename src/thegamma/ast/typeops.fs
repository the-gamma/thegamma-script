module TheGamma.Types
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// Helper functions for type equality and substitution
// ------------------------------------------------------------------------------------------------

type TypeContext = 
  { EquivalentVars : (TypeVar * TypeVar) list }

let rec listsEqual l1 l2 f = 
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys when f x y -> listsEqual xs ys f
  | _ -> false 

let rec arraysEqual (l1:_[]) (l2:_[]) f = 
  let rec loop i =
    if i = l1.Length && i = l2.Length then true
    elif i < l1.Length && i < l2.Length then 
      f (l1.[i]) (l2.[i]) && loop (i+1)
    else false
  loop 0

let rec memberNamesEqual m1 m2 =
  match m1, m2 with 
  | Member.Property(name=n1), Member.Property(name=n2)
  | Member.Method(name=n1), Member.Method(name=n2) -> n1 = n2 
  | _ -> false

let (|BoundTypeVariables|) t = 
  match t with 
  | Type.Forall(vars, _) -> vars, t
  | _ -> [], t

let rec membersEqual ctx m1 m2 =
  match m1, m2 with 
  | Member.Property(n1, t1, _, _), Member.Property(n2, t2, _, _) -> 
      n1 = n2 && typesEqualAux ctx t1 t2
  | Member.Method(n1, a1, BoundTypeVariables (v1, r1), _, _), Member.Method(n2, a2, BoundTypeVariables (v2, r2), _, _) -> 
      let ctx = { ctx with EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      n1 = n2 && typesEqualAux ctx r1 r2 && 
        listsEqual a1 a2 (fun (s1, b1, t1) (s2, b2, t2) -> 
          s1 = s2 && b1 = b2 && typesEqualAux ctx t1 t2)
  | _ -> false

and typesEqualAux ctx t1 t2 = 
  match t1, t2 with
  | Type.Any, _ | _, Type.Any -> true
  | Type.Parameter(p1), Type.Parameter(p2) ->
      ctx.EquivalentVars |> List.exists (fun (l, r) -> l = p1 && r = p2)
  | Type.Delayed(g1, _), Type.Delayed(g2, _) -> g1 = g2
  | Type.List t1, Type.List t2 -> typesEqualAux ctx t1 t2
  | Type.Function(a1, r1), Type.Function(a2, r2) -> 
      listsEqual (r1::a1) (r2::a2) (typesEqualAux ctx)
  | Type.Object(o1), Type.Object(o2) -> arraysEqual o1.Members o2.Members (membersEqual ctx)
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2  
  | Type.Forall(v1, t1), Type.Forall(v2, t2) when List.length v1 = List.length v2 ->
      let ctx = { ctx with EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      typesEqualAux ctx t1 t2
  | Type.App(t1, ts1), Type.App(t2, ts2) when List.length ts1 = List.length ts2 ->
      (t1, t2)::(List.zip ts1 ts2) |> List.forall (fun (t1, t2) -> typesEqualAux ctx t1 t2)
  | _ -> false

/// Returns true when closed types have equivalent structure up to renaming of local type variables
let typesEqual = typesEqualAux { EquivalentVars = [] }

let rec substituteMembers assigns members = 
  members |> Array.map (function
    | Member.Method(n,ars,BoundTypeVariables (vars, t),d,e) -> 
        // Generic methods are encoded as methods with forall return type
        // but we need to avoid substituting in parameters too!
        let assigns = vars |> List.fold (fun assigns var -> Map.remove var assigns) assigns
        let ars = ars |> List.map (fun (n,o,t) -> n, o, substituteTypes assigns t)
        Member.Method(n,ars,substituteTypes assigns t,d,e)
    | Member.Property(n,t,m,e) -> Member.Property(n,substituteTypes assigns t,m,e))      

/// Substitute types for type variables in a given type
and substituteTypes assigns t =
  match t with
  | Type.Parameter s when Map.containsKey s assigns -> assigns.[s]
  | Type.Parameter _ | Type.Any | Type.Primitive _ -> t
  | Type.Function(ts, t) -> Type.Function(List.map (substituteTypes assigns) ts, substituteTypes assigns t)
  | Type.List(t) -> Type.List(substituteTypes assigns t)
  | Type.Object(o) -> { Members = substituteMembers assigns o.Members } |> Type.Object
  | Type.Delayed(g, f) -> 
      let f = 
        { new Future<Type> with 
            member x.Then(g) =             
              f.Then(fun t -> g (substituteTypes assigns t)) }
      Type.Delayed(g, f)
  | Type.App(t, ts) -> Type.App(substituteTypes assigns t, List.map (substituteTypes assigns) ts)
  | Type.Forall(vars, t) ->
      let assigns = vars |> List.fold (fun assigns var -> Map.remove var assigns) assigns
      Type.Forall(vars, substituteTypes assigns t)

type UnifictionContext = 
  { FreeVars : Set<TypeVar>
    Assignments : (TypeVar * Type) list
    EquivalentVars : (TypeVar * TypeVar) list 
    Errors : (Type * Type) list }

let rec unifyTypesAux ctx ts1 ts2 =
  match ts1, ts2 with
  | Type.Parameter n::ts1, t::ts2 when 
        ( ctx.FreeVars.Contains n &&
          match t with Type.Parameter _ -> false | _ -> true ) ->
      unifyTypesAux { ctx with Assignments = (n, t)::ctx.Assignments } ts1 ts2
  | Type.Function(tis1, to1)::ts1, Type.Function(tis2, to2)::ts2 ->
      unifyTypesAux ctx (to1::tis1 @ ts1) (to2::tis2 @ ts2)
  | Type.Object({ Members = m1 })::ts1, Type.Object({ Members = m2 })::ts2 
      when arraysEqual m1 m2 memberNamesEqual ->
        unifyTypesAux ctx ts1 ts2
  | Type.List(t1)::ts1, Type.List(t2)::ts2 -> 
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.Forall(v1, t1)::ts1, Type.Forall(v2, t2)::ts2 when List.length v1 = List.length v2 ->
      let ctx = { ctx with UnifictionContext.EquivalentVars = List.append (List.zip v1 v2) ctx.EquivalentVars }
      unifyTypesAux ctx (t1::ts1) (t2::ts2)
  | Type.App(t1, ta1)::tb1, Type.App(t2, ta2)::tb2 when List.length ta1 = List.length ta2 ->
      unifyTypesAux ctx (t1::(List.append ta1 tb1)) (t2::(List.append ta2 tb2))
  | t1::ts1, t2::ts2 when typesEqualAux { EquivalentVars = ctx.EquivalentVars } t1 t2 ->
      unifyTypesAux ctx ts1 ts2  
  | t1::ts1, t2::ts2 -> 
      unifyTypesAux { ctx with Errors = (t1, t2)::ctx.Errors } ts1 ts2
  | [], [] -> ctx
  | _ -> failwith "unifyTypesAux: The lists of types had mismatching lengths"

/// Unify a type with given free type variables with a given closed type
/// and return assignments (possibly conflicting) with mismatching types 
let unifyTypes free ts1 ts2 = 
  let ctx = { FreeVars = set free; Assignments = []; EquivalentVars = []; Errors = [] }
  let ctx = unifyTypesAux ctx [ts1] [ts2]
  ctx.Assignments, ctx.Errors

/// Perform type applications 
let rec reduceType t = 
  match t with
  | Type.App(Type.Forall(vars, t), args) ->
      if List.length vars <> List.length args then failwith "reduceType: Invalid type application"
      let t = substituteTypes (Map.ofList (List.zip vars args)) t
      reduceType t 
  | _ -> t