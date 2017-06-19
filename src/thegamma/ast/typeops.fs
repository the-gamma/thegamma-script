module TheGamma.Types
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// Helper functions for working with types
// ------------------------------------------------------------------------------------------------

let rec listsEqual l1 l2 f = 
  match l1, l2 with
  | [], [] -> true
  | x::xs, y::ys when f x y -> listsEqual xs ys f
  | _ -> false 

let optionsEqual o1 o2 f = 
  match o1, o2 with
  | None, None -> true
  | Some v1, Some v2 -> f v1 v2
  | _ -> false

let rec typesEqual t1 t2 = 
  match t1, t2 with
  | Type.Any, _ | _, Type.Any -> true
  | Type.List t1, Type.List t2 -> typesEqual t1 t2
  | Type.Method(a1, r1), Type.Method(a2, r2) -> 
      optionsEqual (r1 [for ma in a1 -> ma.Type, None]) (r2 [for ma in a2 -> ma.Type, None]) typesEqual &&
      listsEqual a1 a2 (fun m1 m2 -> m1.Name = m2.Name && m1.Optional = m2.Optional && m1.Static = m2.Static && typesEqual m1.Type m2.Type)
  | Type.Object(o1), Type.Object(o2) -> o1.TypeEquals(o2)
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2  
  | _ -> false
