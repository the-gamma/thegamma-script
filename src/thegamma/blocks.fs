module TheGamma.Blocks

open Fable.Import.Browser
open TheGamma.Common
open TheGamma.Html
open TheGamma.Ast
open TheGamma.TypeProviders.FSharpProvider

open System

let getEntity (svc:Services.CheckingService) code = async {
  let! success, bound, prog = svc.TypeCheck("let it = " + code)
  let _, ent = bound.Entities |> Seq.find (function (_, { Kind = EntityKind.Variable({ Name = "it" }, _) }) -> true | _ -> false)
  let _, prog = bound.Entities |> Seq.find (function (_, { Kind = EntityKind.Program _ }) -> true | _ -> false)
  let errors = TypeChecker.collectTypeErrors prog
  let errors = [ for e in errors -> code.Substring(e.Range.Start, e.Range.End - e.Range.Start + 1), e.Message ]
  return errors, ent }


type Event = 
  | RemoveLast
  | AddElement of string
  | UpdateParameter of string * string

type State = 
  { CheckingService : Services.CheckingService
    Globals : Entity list
    Chain : list<string * (string * string) list option>
    Completions : string[]
    Entity : Entity option }
  static member Create(svc, globals) = 
    { CheckingService = svc; Globals = globals
      Entity = None; Chain = []; Completions = [||] }

let getTypeName state = 
  match state.Entity with
  | Some { Type = Some(Type.Object(:? GenericType as s)) } -> s.TypeDefinition.FullName
  | _ -> "object" 


let formatChain chain = 
  chain 
  |> List.rev 
  |> List.map (fun (id, args) ->
      Ast.escapeIdent id +
      match args with 
      | Some args -> "(" + String.concat ", " (List.map snd args) + ")"
      | None -> "")
  |> String.concat "." 


let updateCompletions state = async {
  let! state = state
  match state with 
  | { Chain = [] } -> 
      return { state with Completions = [| for g in state.Globals -> g.Name |] }
  | { Chain = chain } -> 
      let code = formatChain chain
      let! errs, ent = getEntity state.CheckingService code
      match errs, ent with 
      | [], { Type = Some(Type.Object(obj)) } -> 
          let compls = [| for m in obj.Members do if m.Name <> "preview" then yield m.Name |]
          return { state with Entity = Some ent; Completions = compls }
      | _ -> 
          return { state with Entity = Some ent; Completions = [||] } }


let update state evt = updateCompletions <| async {
  match evt, state with 
  | RemoveLast, { Chain = [] }
  | RemoveLast, { Chain = [_] } -> 
      return { state with Chain = []; Entity = None }
  | RemoveLast, { Chain = _::chain } -> 
      return { state with Chain = chain } 

  | AddElement(m), _ -> 
      let code = formatChain ((m, None)::state.Chain)
      let! errs, ent = getEntity state.CheckingService code
      match ent with 
      | { Type = Some(Type.Method(args, _)) } -> 
          return { state with Chain = (m, Some [ for a in args -> a.Name, "" ])::state.Chain } 
      | _ -> 
          return { state with Chain = (m, None)::state.Chain }

  | UpdateParameter(p, v), { Chain = (m, Some pars)::rest } ->
      let newPars = pars |> List.map (fun (po, vo) -> if p = po then p, v else po, vo)
      return { state with Chain = (m, Some newPars)::rest } 
      
  | _ -> return state }


let tryGetPreview state = 
  let (|Evaluate|) (l:Lazy<_>) = l.Value
  match state.Entity with 
  | Some ent -> 
      Interpreter.evaluate state.Globals ent |> ignore
      match ent.Value with 
      | Some { Preview = Evaluate (Some preview) } -> 
          let mutable node = h?div ["id" => "blockspreview"] [text "Loading preview..."]
          let mutable returned = false
          async { let! nd = table<int, int>.create(unbox<Series.series<string, obj>> preview).render()
                  if returned then nd |> renderTo (document.getElementById "blockspreview")
                  else node <- nd } |> Async.StartImmediate
          returned <- true
          node
      | _ -> h?div [] [text "Preview has no value"]
  | _ -> h?div [] [text "No preview available"]
   

let render trigger state =
  h?div ["class" => "pilleditor"] [
    h?ul ["class" => "pills"] [
      let renderCoreChain (chain:list<_ * _ option>) = 
        [ for id, pars in List.rev chain ->
            h?li [] [ text (id + if pars.IsSome then " (...)" else "") ] ]

      let renderChain removeLast chain = 
        match removeLast, chain with
        | true, (last, _)::rest -> 
            renderCoreChain rest @ 
            [ h?li [] [ text last; h?a ["click" =!> fun _ _ -> trigger RemoveLast ] [ h?i ["class" => "gfa gfa-times"] [] ] ] ]
        | _, rest -> 
            renderCoreChain rest

      match state.Chain with
      | (meth, Some pars)::rest -> yield! renderChain false rest 
      | chain -> yield! renderChain true chain

      match state.Chain with
      | (meth, Some pars)::rest ->
          yield h?li [] [  
            yield text (meth + "(")
            for p, v in pars do 
              yield text (p + " = ") 
              yield h?input ["type"=>"text"; "value"=>v; "input" =!> fun el _ -> 
                trigger (UpdateParameter(p, (unbox<HTMLInputElement> el).value)) ] []
            yield text ")"
            yield h?a ["click" =!> fun _ _ -> trigger RemoveLast ] [ h?i ["class" => "gfa gfa-times"] [] ] 
          ]
      | _ -> ()

      if state.Completions.Length > 0 then
          yield h?li [] [  
            h?div ["class" => "selectwrapper"] [
              h?select [ "change" =!> fun el _ -> trigger(AddElement((unbox<HTMLSelectElement> el).value)) ] [
                yield h?option [] [text "(choose an operation to add)"] 
                for s in state.Completions -> h?option [ "value" => s ] [ text s ]
              ]
            ] 
          ]
    ] 

    tryGetPreview state
    
    h?p [] [ text (getTypeName state) ]
  ]

let createBlockEditor svc globals id = Async.StartImmediate <| async {
  try
    let! globals = globals |> Async.AwaitFuture
    let state = State.Create(svc, globals)
    let! state = 
      //["olympics"; "group data"; "by Games"; "average Year"]
      ["olympics"; "paging"; "take"]
      |> List.fold (fun st s -> async { let! st = st in return! update st (AddElement(s)) }) (async.Return state)
    createVirtualDomAsyncApp id state render update
  with e -> 
    Log.exn("system", "Something went wrong: %O", e)
  }


    


