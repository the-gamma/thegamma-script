module Fable.Helpers.Virtualdom

open Fable.Core
open Fable.Core.JsInterop
open Browser.Types

[<Import("h","virtual-dom")>]
let h(arg1: string, arg2: obj, arg3: obj[]): obj = failwith "JS only"

[<Import("diff","virtual-dom")>]
let diff (tree1:obj) (tree2:obj): obj = failwith "JS only"

[<Import("patch","virtual-dom")>]
let patch (node:obj) (patches:obj): Node = failwith "JS only"

[<Import("create","virtual-dom")>]
let createElement (e:obj): Node = failwith "JS only"
