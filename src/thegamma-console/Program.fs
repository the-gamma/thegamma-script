open TheGamma
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders
open TheGamma.TypeProviders.Pivot

// ------------------------------------------------------------------------------------------------
// Fake series and fake pivot type
// ------------------------------------------------------------------------------------------------

type Series(x, kind:string) = 
  member z.Kind = kind
  interface FSharpProvider.GenericType with 
    member z.Members = [| |]
    member z.TypeEquals t = false
    member z.TypeArguments = []
    member z.TypeDefinition = x 

let series =
  { new FSharpProvider.GenericTypeDefinition with
    member x.Members = [| |]
    member x.TypeEquals t = false
    member x.FullName = "series"
    member x.TypeParameterCount = 2
    member x.Apply tys = 
      { new FSharpProvider.GenericTypeSchema with 
          member y.Members = [| |]
          member y.TypeEquals t = false
          member y.TypeDefinition = x
          member y.TypeArguments = tys
          member y.Substitute _ =
            let isTable = 
              match tys with 
              | [ FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number | PrimitiveType.Date)); 
                  FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number)) ] -> "line"
              | [ _; FSharpProvider.TypeSchema.Primitive(Type.Primitive(PrimitiveType.Number)) ] -> "column"
              | [_; FSharpProvider.TypeSchema.Primitive(Type.Object(:? PivotObject))] -> "table"
              | _ -> "object"
            Series(x, isTable) :> _ } }

let types = System.Collections.Generic.Dictionary<_, _>()
let lookupNamed n = types.[n]
types.Add("series", Type.Object series)

let olympics = 
  makePivotGlobalValue "https://thegamma-services.azurewebsites.net/pdata/olympics" "olympics" lookupNamed false
    [ "Games", PrimitiveType.String
      "Year", PrimitiveType.Number
      "Sport", PrimitiveType.String
      "Discipline", PrimitiveType.String
      "Athlete", PrimitiveType.String
      "Team", PrimitiveType.String
      "Gender", PrimitiveType.String
      "Event", PrimitiveType.String
      "Medal", PrimitiveType.String
      "Gold", PrimitiveType.Number
      "Silver", PrimitiveType.Number
      "Bronze", PrimitiveType.Number ]

let typ = 
  match olympics with
  | TypeProviders.ProvidedType.GlobalValue(_, _, _, typ) -> typ 
  | _ -> failwith "makePivotGlobalValue did not return type"
types.Add("olympics", typ)

let vars = [ "olympics", typ ]

// ------------------------------------------------------------------------------------------------
// Testing the type checker & Pivot type provider
// ------------------------------------------------------------------------------------------------

let template = """
<!DOCTYPE html>
<html lang="en-us">
<body>

<script type="text/thegamma" id="s57-code">[code]</script>
<div class="thegamma" id="s57"></div>

<script type="text/javascript">
if (!thegamma) { var thegamma=[],tg="https://thegamma.net/lib/thegamma-0.1/";
["//www.google.com/jsapi",tg+"babel.min.js",tg+"core-js.min.js",tg+"require.js",tg+"embed.js"].
forEach(function(u) { document.write('<sc'+'ript type="text/javascript" src="'+u+'"></sc'+'ript>'); });
} thegamma.push("s57");
</script>

</body>
</html>"""

open System

type Resuult<'T, 'E> =
  | Success of 'T
  | Error of 'E

Log.setEnabled Set.empty 
let ctx = Binder.createContext [] "script"

let getType code = async {
  let code = "let it = " + code
  let prog, _ = code |> Parser.parseProgram
  let prog, entities = Binder.bindProgram ctx prog
  let globals = [ for n, t in vars -> Interpreter.globalEntity n [] t None ] 
  do! TypeChecker.typeCheckProgram globals entities (Interpreter.evaluate globals) prog
  let _, ent = entities.Entities |> Seq.find (function (_, { Kind = EntityKind.Variable({ Name = "it" }, _) }) -> true | _ -> false)
  let errors = TypeChecker.collectTypeErrors prog
  let errors = [ for e in errors -> code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1), e.Message ]
  return errors, ent.Type.Value }

let getTypeName code = async {
  let! _, typ = getType code 
  match typ with
  | Type.Object(:? Series as s) -> return s.Kind
  | _ -> return "object" }

let getCompletions code = async {
  let! errors, typ = getType code
  match errors, typ with
  | [], Type.Object(obj) -> return [| for m in obj.Members do if m.Name <> "preview" then yield Ast.escapeIdent m.Name |]
  | [], _ -> return failwith "Not an object"
  | _::_, _ -> return failwith "There were errors" }

let selectMember code members =
  let rec loop index =
    Console.Clear()
    printfn "CODE:\n  %s\nSELECT:" code
    members |> Array.iteri (fun i m ->
      if i = index then printfn "  -> %s" m else printfn "     %s" m)
    let key = Console.ReadKey().Key
    match key with
    | ConsoleKey.UpArrow when members.Length > 0 -> loop ((index - 1 + members.Length) % members.Length)
    | ConsoleKey.DownArrow when members.Length > 0 -> loop ((index + 1) % members.Length)
    | ConsoleKey.Enter when members.Length > 0 -> Some(members.[index])
    | ConsoleKey.Backspace -> None
    | _ -> loop index
  loop 0

let chain = ResizeArray ["olympics"]
while true do  
  let code = chain |> String.concat "."
  let kind = getTypeName code |> Async.RunSynchronously 
  if kind <> "object" then
    let code = 
      match kind with
      | "table" -> sprintf "let data = %s\ntable.create(data)" code
      | "column" -> sprintf "let data = %s\ncompost.charts.column(data)" code
      | "line" -> sprintf "let data = %s\ncompost.charts.line(data)" code
      | _ -> failwith "Unexpected kind"
    let file = IO.Path.GetTempFileName()
    IO.File.WriteAllText(file + ".html", template.Replace("[code]", code))
    Diagnostics.Process.Start(file + ".html") |> ignore
  
  let mem = selectMember code (getCompletions code |> Async.RunSynchronously)
  match mem with
  | Some(op & ("take" | "skip")) -> chain.Add(op + "(10)")
  | Some m -> chain.Add(m)
  | None when chain.Count > 1 -> chain.RemoveAt(chain.Count-1)
  | _ -> ()

      


