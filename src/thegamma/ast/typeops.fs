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

let rec typesEqual t1 t2 = 
  match t1, t2 with
  | Type.Any, _ | _, Type.Any -> true
  | Type.List t1, Type.List t2 -> typesEqual t1 t2
  | Type.Function(a1, r1), Type.Function(a2, r2) -> listsEqual (r1::a1) (r2::a2) typesEqual
  | Type.Object(o1), Type.Object(o2) -> o1.TypeEquals(o2)
  | Type.Primitive n1, Type.Primitive n2 -> n1 = n2  
  | _ -> false
