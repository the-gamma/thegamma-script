module Fable.Helpers.Babel

open Fable.Core

[<Emit("eval($0)")>]
let eval (s:string) : 'T = failwith "JS only"

type BabelOptions = 
  { presets : string[] }

type BabelResult = 
  { code : string }

type Babel =
  abstract transformFromAst : obj * string * BabelOptions -> BabelResult

[<Emit("Babel")>]
let babel : Babel = Unchecked.defaultof<_> 
