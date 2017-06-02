open System.Reflection
open Microsoft.FSharp.Reflection

type Type = interface end

type GenericParameterType = 
  { kind : string 
    name : string }
  interface Type
  static member Create(n) = { kind = "parameter"; name = n } :> Type

type PrimitiveType = 
  { kind : string 
    name : string }
  interface Type
  static member Create(n) = { kind = "primitive"; name = n } :> Type

type FunctionType = 
  { kind : string 
    arguments : Type[]
    returns : Type }
  interface Type
  static member Create(a,r) = { kind = "function"; arguments = Array.ofSeq a; returns = r } :> Type 

type ArrayType = 
  { kind : string 
    element : Type }
  interface Type
  static member Create(e) = { kind = "array"; element = e } :> Type 

type NamedType = 
  { kind : string 
    name : string
    typargs : Type[] }
  interface Type
  static member Create(n,t) = { kind = "named"; name = n; typargs = Array.ofSeq t } :> Type 
  
type Member = interface end

type Argument = 
  { name : string
    optional : bool
    ``type`` : Type }

type MethodMember = 
  { kind : string
    name : string 
    typepars : Type[]
    arguments:Argument[]
    returns:Type }
  interface Member
  static member Create(m, t, a, r) =
    { kind = "method"; name = m; typepars = Array.ofSeq t; arguments = Array.ofSeq a; returns = r } :> Member

type PropertyMember = 
  { kind : string
    name : string 
    returns:Type }
  interface Member
  static member Create(m, r) =
    { kind = "property"; name = m; returns = r } :> Member

type ExportedType = 
  { name : string
    typepars : Type[]
    ``static`` : bool 
    instance : string[]
    members : Member[] }

type Context = 
  { KnownTypes : Map<string, string> }

let rec getFunctionTypes ctx acc typ = 
  let inp, out = FSharpType.GetFunctionElements(typ)
  if out.Name = "FSharpFunc`2" then getFunctionTypes ctx (inp::acc) out
  else List.rev (inp::acc), out

let rec formatType ctx (genPars:Map<_, _>) (typ:System.Type) = 
  match typ.Name with
  | _ when typ.IsGenericParameter -> genPars.[typ.Name]
  | n when ctx.KnownTypes.ContainsKey n -> 
      let typars = if typ.IsGenericType then typ.GetGenericArguments() else [||]
      let nice = ctx.KnownTypes.[n]
      NamedType.Create(nice, typars |> Array.map (formatType ctx genPars) |> List.ofArray)
  | _ when typ.IsArray -> 
      ArrayType.Create(typ.GetElementType() |> formatType ctx genPars)      
  | "Boolean" -> PrimitiveType.Create("bool")
  | "String" -> PrimitiveType.Create("string")
  | "Double" -> PrimitiveType.Create("float")
  | "DateTime" -> PrimitiveType.Create("date")
  | "Object" -> PrimitiveType.Create("object")
  | "Int32" -> PrimitiveType.Create("int")
  | "Void" -> PrimitiveType.Create("unit")
  | "Unit" -> PrimitiveType.Create("unit")
  | "Tuple`2" -> PrimitiveType.Create("object") // TODO: Something clever
  | "FSharpOption`1" -> PrimitiveType.Create("object") // TODO: Something clever
  | "FSharpFunc`2" -> 
      let inp, out = getFunctionTypes ctx [] typ
      FunctionType.Create(List.map (formatType ctx genPars) inp, formatType ctx genPars out)
  | n -> failwithf "Not supported type: >>>%s<<<" n


let exportType ctx (typ:System.Type) kind = 
  let isStatic = kind &&& BindingFlags.Static = BindingFlags.Static

  let typArgs = 
    if typ.IsGenericType then
      typ.GetGenericArguments()
      |> Array.map (fun t -> t.Name, GenericParameterType.Create(t.Name))
      |> Map.ofSeq
    else Map.empty

  let methods = 
    [ for m in typ.GetMethods(BindingFlags.DeclaredOnly ||| kind ||| BindingFlags.Public) do
        if not m.IsSpecialName && m.Name <> "Equals" && m.Name <> "GetHashCode" then
          let typArgsSrc = if m.IsGenericMethod then m.GetGenericArguments() else [||]
          let typArgsNew = typArgsSrc |> Array.fold (fun typArgs t -> Map.add t.Name (GenericParameterType.Create(t.Name)) typArgs) typArgs

          printfn "%s.%s" typ.Name m.Name
          let pars = 
            [ for p in m.GetParameters() ->
                let opt, typ =
                  if p.ParameterType.Name <> "FSharpOption`1" then false, p.ParameterType
                  else true, p.ParameterType.GetGenericArguments().[0]
                { Argument.name = p.Name; optional = opt; ``type`` = formatType ctx typArgsNew typ } ]

          yield 
            MethodMember.Create
              ( m.Name, [ for t in typArgsSrc -> GenericParameterType.Create(t.Name) ], 
                pars, formatType ctx typArgsNew m.ReturnType) ]

  let properties = 
    [ for m in typ.GetProperties(BindingFlags.DeclaredOnly ||| kind ||| BindingFlags.Public) do
        if not m.IsSpecialName then
          printfn "%s.%s" typ.Name m.Name
          yield PropertyMember.Create( m.Name, formatType ctx typArgs m.PropertyType) ]

  { typepars = [| for t in typArgs -> GenericParameterType.Create(t.Key) |]
    ``static`` = isStatic; instance = [||]
    members = Array.ofSeq (methods  @ properties)
    name = defaultArg (ctx.KnownTypes.TryFind(typ.Name)) typ.Name }



#r "../packages/Newtonsoft.Json/lib/net40/Newtonsoft.Json.dll"
open Newtonsoft.Json

let serializer = JsonSerializer.Create()

let toJson value = 
  let sb = System.Text.StringBuilder()
  use tw = new System.IO.StringWriter(sb)
  serializer.Serialize(tw, value)
  sb.ToString() 

#r "../src/libraries/bin/Debug/bindings.dll"
#r "../src/libraries/bin/Debug/libraries.dll"
#r "../src/libraries/bin/Debug/Fable.Core.dll"
let libs = __SOURCE_DIRECTORY__ + "/../src/libraries/bin/Debug/libraries.dll"
let asm = Assembly.LoadFile(libs)

let chartTypes = 
  [ for t in asm.GetTypes() do 
      if t.FullName.StartsWith("TheGamma.GoogleCharts.Extensions+") &&
        not (t.FullName.Contains("@")) &&
        not (t.Name = "options") then yield t 
    for t in asm.GetTypes() do 
      if t.FullName.StartsWith("TheGamma.GoogleCharts.Options+") then
        yield t ]

let recordTypes = 
  [ for t in asm.GetTypes() do 
      if t.FullName.StartsWith("TheGamma.GoogleCharts.Options+") then yield t ]

let ignoredTypes = 
  [ "IEnumerable`1"; "FSharpAsync`1"; "ChartData"; "IComparer"; "IEqualityComparer"; "DomNode";
    "YouGuessColsBarsKind"; "FSharpSet`1"; "FSharpList`1" ]
        
let knownTypes = 
  [ yield! 
      [ "table`2", "table"
        "placeholder", "placeholder"
        "series`2", "series"

        //"youguess", "youguess"
        "YouGuessLine", "YouGuessLine"
        "YouGuessSortBars", "YouGuessSortBars"
        "YouGuessColsBars", "YouGuessColsBars"

        "AxisOptions", "AxisOptions"
        "ChartOptions", "ChartOptions"
        "CompostBubblesChartSet", "CompostBubblesChartSet"
        "CompostBubblesChart`2", "CompostBubblesChart"
        "CompostColBarChart", "CompostColBarChart"
        "CompostLineChart", "CompostLineChart"
        "CompostLinesChart", "CompostLinesChart"
        "CompostCharts", "CompostCharts"
        //"value`1", "value"
        //"options", "options" 
        ]
    for t in ignoredTypes do yield t, "object"
    for t in recordTypes do yield t.Name, t.Name 
    for t in chartTypes do yield t.Name, t.Name ] |> Map.ofSeq 

let ctx = { KnownTypes = knownTypes }

let e = 
  [|  for ct in chartTypes do
        yield exportType ctx ct BindingFlags.Instance
      yield { exportType ctx (asm.GetType("TheGamma.GoogleCharts.chart")) BindingFlags.Static 
                with instance = [| "chart" |] }
      yield { exportType ctx (asm.GetType("TheGamma.placeholder")) BindingFlags.Static 
                with instance = [| "placeholder" |] }
      yield exportType ctx (asm.GetType("TheGamma.placeholder")) BindingFlags.Instance
      yield { exportType ctx (asm.GetType("TheGamma.table`2")) BindingFlags.Static 
                with instance = [| "table" |] }
      yield exportType ctx (asm.GetType("TheGamma.table`2")) BindingFlags.Instance

      yield exportType ctx (asm.GetType("TheGamma.Interactive.YouGuessLine")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.YouGuessColsBars")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.YouGuessSortBars")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.youguess")) BindingFlags.Instance 
      yield { exportType ctx (asm.GetType("TheGamma.Interactive.youguess")) BindingFlags.Static 
                with instance = [| "youguess" |] }
            
      yield exportType ctx (asm.GetType("TheGamma.Interactive.LegendOptions")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.AxisOptions")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.ChartOptions")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostBubblesChartSet")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostBubblesChart`2")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostColBarChart")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostLineChart")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostLinesChart")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.CompostCharts")) BindingFlags.Instance 
      yield exportType ctx (asm.GetType("TheGamma.Interactive.compost")) BindingFlags.Instance 
      yield { exportType ctx (asm.GetType("TheGamma.Interactive.compost")) BindingFlags.Static 
                with instance = [| "compost" |] }

      yield { exportType ctx (asm.GetType("TheGamma.General.date")) BindingFlags.Static 
                with instance = [| "date$1" |] }
      yield { exportType ctx (asm.GetType("TheGamma.General.math")) BindingFlags.Static 
                with instance = [| "math" |] }

      yield exportType ctx (asm.GetType("TheGamma.Series.series`2")) BindingFlags.Instance 
      yield { exportType ctx (asm.GetType("TheGamma.Series.series`2")) BindingFlags.Static 
                with instance = [| "series" |] }

      yield { exportType ctx (asm.GetType("TheGamma.html")) BindingFlags.Static 
                with instance = [| "html" |] } |]

let fsprovider1 = __SOURCE_DIRECTORY__ + "/../dist"
System.IO.Directory.CreateDirectory(fsprovider1)
System.IO.File.WriteAllText(fsprovider1 + "/libraries.json", toJson e)

// let fsprovider2 = __SOURCE_DIRECTORY__ + "/../out"
// System.IO.Directory.CreateDirectory(fsprovider2)
// System.IO.File.WriteAllText(fsprovider2 + "/libraries.json", toJson e)



(*
for m in typ.GetProperties(BindingFlags.DeclaredOnly ||| BindingFlags.Instance ||| BindingFlags.Public) do
  if not m.IsSpecialName then
    printf "%s : %s" m.Name m.PropertyType.Name


let typ = asm.GetType("TheGamma.GoogleCharts.chart")

for m in typ.GetMembers(BindingFlags.DeclaredOnly ||| BindingFlags.Static ||| BindingFlags.Public) do
  printfn "%s" m.Name


*)
