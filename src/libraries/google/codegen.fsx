// --------------------------------------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------------------------------------

#load "options.fs"
open System
open System.Reflection
open Microsoft.FSharp.Reflection

let gc = Assembly.GetExecutingAssembly().GetTypes() |> Seq.find (fun t -> t.Name = "Options")
let types = gc.GetNestedTypes()

let optionsTypes = types |> Seq.filter (fun t -> t.Name.EndsWith("Options"))
let otherTypes = types |> Seq.filter (fun t -> not (t.Name.EndsWith("Options")))

let camelCase (name:string) = 
  name.[0].ToString().ToLower() + name.Substring(1)
let dropSuffixes (name:string) =
  name.Replace("ChartOptions", "").Replace("Options", "")

let primitiveTypeNames = 
  dict [ typeof<float>, "float"; typeof<bool>, "bool"; typeof<obj>, "obj"; typeof<DateTime>, "DateTime"
         typeof<string>, "string"; typeof<string[]>, "string[]"; typeof<float[]>, "float[]"; typeof<obj[]>, "obj[]" ]

let primitiveTypes = 
  set [ for t in primitiveTypeNames.Keys -> t.FullName ]

let getOptions t =
  [ for f in FSharpType.GetRecordFields(t) do
      let typ = f.PropertyType
      let isPrimitive = primitiveTypes.Contains(typ.FullName)
      let isNested = FSharpType.IsRecord(typ)
      if isPrimitive then yield Choice1Of3(f.Name, typ)
      elif isNested then yield Choice2Of3(f.Name, typ)
      else yield Choice3Of3(f.Name, typ) ]

let getAllOptions t =
  getOptions t |> List.map (function Choice1Of3 v | Choice2Of3 v | Choice3Of3 v -> v)

let getPrimitivieNestedOtherOptions t =
  let options = getOptions t
  options |> List.choose (function Choice1Of3 v -> Some v | _ -> None),
  options |> List.choose (function Choice2Of3 v -> Some v | _ -> None),
  options |> List.choose (function Choice3Of3 v -> Some v | _ -> None)

let rec formatInputType (t:System.Type) =
  if t.IsArray then "seq<" + (formatInputType (t.GetElementType())) + ">"
  elif primitiveTypeNames.ContainsKey(t) then primitiveTypeNames.[t]
  else t.Name

let safeName s = if s = "type" then "``type``" else s

let formatParameters primitive =
  [ for n, t in primitive -> 
      sprintf "?%s:%s" (safeName n) (formatInputType t) ]
  |> String.concat ","

let getConversion (t:System.Type) =
  if t.IsArray then Some("Array.ofSeq")
  else None

let formatSetters primitive = 
  [ for n, t in primitive -> 
      let converted =
        match getConversion t with
        | Some f -> sprintf "(Option.map %s %s)" f (safeName n)
        | _ -> n
      sprintf "%s = right o \"%s\" %s" (safeName n) n converted ]
  |> String.concat "; "

let formatCopies nested = 
  [ for n, t in nested -> 
      sprintf "%s = copy o \"%s\"" n n  ]
  |> String.concat "; "

let writeSetters (wr:IO.TextWriter) =
  for t in optionsTypes do
    let primitive, nested, other = getPrimitivieNestedOtherOptions t
    let pars = formatParameters primitive
    let sets = formatSetters primitive    
    fprintfn wr "type %s = " (dropSuffixes t.Name)
    fprintfn wr "  { data : ChartData; typeName : string; "
    fprintfn wr "    options : %s }" t.Name
    fprintfn wr "  interface Chart"
    fprintfn wr "  member x.show() = Helpers.showChart(x)" 
    fprintfn wr "  member x.set(%s) = " pars
    fprintfn wr "    let o = x.options"
    fprintfn wr "    let newOptions = { x.options with %s }" sets
    fprintfn wr "    { x with options = newOptions }"

    for name, otyp in other do
      let pars = formatParameters [name, otyp]
      let sets = formatSetters [name, otyp]
      fprintfn wr "  member x.%s(%s) =" name pars
      fprintfn wr "    let o = x.options" 
      fprintfn wr "    { x with options = { x.options with %s } }" sets
    
    for name, ntyp in nested do
      let primitive, nested, other = getPrimitivieNestedOtherOptions ntyp
      if other <> [] then failwith "!"
      let pars = formatParameters primitive
      let sets = formatSetters primitive   
      let copies = formatCopies nested
      if pars <> "" then 
        fprintfn wr "  member x.%s(%s) =" name pars
        fprintfn wr "    let o = x.options.%s" name
        fprintfn wr "    let newNested = { %s.%s; %s }" ntyp.Name sets copies
        fprintfn wr "    { x with options = { x.options with %s = newNested } }" name
    

let formatInitializers primitive = 
  [ for n, t in primitive -> 
      let converted =
        match getConversion t with
        | Some f -> sprintf "(Option.map %s %s)" f (safeName n)
        | _ -> (safeName n)
      sprintf "%s = orDefault %s" (safeName n) converted ]
  |> String.concat "; "

let writeOtherOptions (wr:IO.TextWriter) = 
  fprintfn wr "type options ="
  for t in otherTypes do
    let opts = getAllOptions t
    let pars = formatParameters opts
    let inits = formatInitializers opts
    fprintfn wr "  static member %s(%s) =" (camelCase t.Name) pars
    fprintfn wr "    { %s.%s }" t.Name inits 


let writeChartType (wr:IO.TextWriter) =
  for t in optionsTypes do
    let opts = [ for n, _ in getAllOptions t -> sprintf "%s = undefined<_>()" n ]
    let optsRest = opts |> String.concat "; "

    fprintfn wr "type %s with" t.Name
    fprintfn wr "  static member empty ="
    fprintfn wr "    { %s.%s }" t.Name optsRest
  

let write() =
  use fs = IO.File.Create(__SOURCE_DIRECTORY__ + "/extensions.fs")
  use fw = new IO.StreamWriter(fs)
  fprintfn fw "[<ReflectedDefinition;AutoOpen>]"
  fprintfn fw "module TheGamma.GoogleCharts.Extensions"
  fprintfn fw ""
  fprintfn fw "open System"
  fprintfn fw "open TheGamma.GoogleCharts"
  fprintfn fw "open TheGamma.GoogleCharts.Helpers"
  fprintfn fw "open TheGamma.GoogleCharts.Options"
  fprintfn fw ""
  writeSetters fw
  fprintfn fw ""
  writeChartType fw
  fprintfn fw ""
  writeOtherOptions fw

write()