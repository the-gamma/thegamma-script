#nowarn "40"
#r "bin/Debug/thegamma.dll"
open TheGamma



let rec seriesTy = Delayed(fun () -> 
  Type.Object 
    { Members = 
      [ Member.Method("sortValues", ["reverse", Type.Primitive "bool"], seriesTy)
        Member.Method("take", ["count", Type.Primitive "num"], seriesTy) ]
    })

let worldTy = 
  Type.Object
    { Members = 
        [ Member.Property("CO2 emissions (kt)", seriesTy) ] }

let globals = Map.ofSeq [ "world", worldTy ]


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

let typed = TypeChecker.typeCheck { Globals = globals } expr        
TypeChecker.asObjectType typed.Type

let getObjectType e = (Option.get e).Type |> TypeChecker.asObjectType

typeBefore { Line=1; Column=6 } None typed |> getObjectType
typeBefore { Line=1; Column=25 } None typed |> getObjectType
typeBefore { Line=1; Column=26 } None typed |> getObjectType
(typeBefore { Line=2; Column=24 } None typed |> Option.get).Type

typed.Range