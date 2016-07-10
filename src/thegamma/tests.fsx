#nowarn "40"
#r "bin/Debug/thegamma.dll"
open TheGamma

Main.typeCheck "foo(1)" |> Async.RunSynchronously
Main.typeCheck "foo(1\n.bar()\n.zoo()" |> Async.RunSynchronously


let code = "world." 
let errs, ty = Main.typeCheck code |> Async.RunSynchronously
let info = TypeChecker.collectEditorInfo { Completions = [] } ty |> Async.RunSynchronously
info.Completions |> Array.ofSeq

code.[5]
code.[30 .. 34-1]
code.[41 .. 41-1]
code.[6 .. 26-1]

(*
let olympicsTy = TypePoviders.createRestType "http://127.0.0.1:10051" "/"
let globals = Map.ofSeq [ "olympics", olympicsTy; "world", TypePoviders.worldTy ]


let sample = """world.'CO2 emissions (kt)'
.sortValues(reverse=true)
.take(10)"""

let sample1 = sample.Replace("\r\n", "\n")
let (Parsec.Parser p) = Tokenizer.tokens
let expr = 
  let (Some([], r)) = p (List.ofSeq sample1) 
  let r = r |> List.filter (function { Token = TokenKind.White _ } -> false | _ -> true)
  let (Parsec.Parser q) = Parser.expression
  q r |> Option.get |> snd

let typed = TypeChecker.typeCheck { Globals = globals } expr  |> Async.RunSynchronously

TypeChecker.asObjectType typed.Type
|> Async.RunSynchronously

let getObjectType e = (Option.get e).Type |> TypeChecker.asObjectType

typeBefore { Line=1; Column=6 } None typed |> getObjectType
typeBefore { Line=1; Column=25 } None typed |> getObjectType
typeBefore { Line=1; Column=26 } None typed |> getObjectType
(typeBefore { Line=2; Column=24 } None typed |> Option.get).Type

typed.Range
*)