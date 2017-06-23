// ------------------------------------------------------------------------------------------------
// Live preview for anything with 'show' method
// ------------------------------------------------------------------------------------------------
module TheGamma.Live.Showable

open Fable.Core
open Fable.Import
open Fable.Import.Browser

open TheGamma
open TheGamma.Ast
open TheGamma.Html
open TheGamma.Live.Common
open TheGamma.Common
open TheGamma.TypeChecker
open TheGamma.TypeProviders

module FsOption = Microsoft.FSharp.Core.Option

// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

let commandAtLocation loc (program:Program) =
  program.Body.Node |> List.tryFind (fun cmd ->
    cmd.Range.Start <= loc && cmd.Range.End + 1 >= loc)

type ShowableEditorState = 
  { EndLocation : int
    Preview : DomNode 
    PreviewSymbol : Symbol
    PreviewID : int
    }

[<Emit("$0.show($1)")>]
let callShow (inst:obj) (id:string) : unit = failwith "JS"

let (|HasShow|_|) = function
  | Type.Object obj ->
      let hasShow = obj.Members |> Array.exists (function 
        | { Name="show"; Type=Type.Method([{ Type = Type.Primitive PrimitiveType.String }], _) } -> true
        | _ -> false)
      if hasShow then Some() else None
  | _ -> None

let updateBody trigger state = 
  Log.trace("live", "Showable - updating body")
  match commandAtLocation state.Location state.Program with
  | Some({ Node = Command.Let(_, e); Entity = Some { Kind = EntityKind.LetCommand(_, ent) } } as cmd) 
  | Some({ Node = Command.Expr e; Entity = Some { Kind = EntityKind.RunCommand ent } } as cmd) ->
      let chain = collectFirstChain e
      match ent.Type.Value, chain with 
      | HasShow, _ ->
          match Interpreter.evaluate state.Globals ent with 
          | Some res ->
              let id = 
                if ent.Symbol <> state.State.PreviewSymbol then state.State.PreviewID + 1
                else state.State.PreviewID
              let placeholder = h?div ["class"=>"placeholder"] [text "Loading preview..."]
              let dom = h?div [] [h.delayed (string id) placeholder (fun id ->            
                Log.trace("live", "Show: %O", res.Value)
                callShow res.Value id
              )]
              Some { state with State = { PreviewSymbol = ent.Symbol; PreviewID = id; EndLocation = cmd.Range.End; Preview = dom } }
          | _ -> None
            
      | _, Some chain -> 
          chain.Chain |> Seq.sortByDescending fst |> Seq.tryPick (fun (_, node) ->
            let nm = { Name.Name="preview" }
            match node.Entity.Value.Type with
            | Some(Type.Object(FindMember nm m)) ->
                match pickMetaByType "http://schema.org" "WebPage" m.Metadata with
                | Some meta ->  
                    let url = getProperty meta "url"
                    let dom = h?iframe [ "src" => url ] [] 
                    let id = state.State.PreviewID + 1
                    Some { state with State = { PreviewSymbol = ent.Symbol; PreviewID = id; EndLocation = cmd.Range.End; Preview = dom } }
                | _ -> None
            | _ -> None)

      | _ -> None
  | _ -> None


let rec updateShowableState trigger state event = 
  match event with
  | UpdateSource _ 
  | UpdateLocation _ -> state |> updateBody trigger 
  | InitializeGlobals _ 
  | CustomEvent () -> Some state 

let renderShowable trigger (state:LiveState<_>) = 
  let endLine, _ = state.Mapper.AbsoluteToLineCol(state.State.EndLocation)
  let dom = 
    h?div [ "class" => "pivot-preview" ] [
      h?ul ["class" => "tabs"] [
        h?li ["class" => "selected"] [ h?a [] [ text "preview" ] ]
      ]
      h?div ["class" => "preview-body"] [
        yield state.State.Preview
      ] 
    ]
  Some { Line = endLine; Preview = dom }


// ------------------------------------------------------------------------------------------------
//
// ------------------------------------------------------------------------------------------------

let preview = 
  { ID = "Showable"
    Update = updateShowableState
    Render = renderShowable
    InitialState = { PreviewID = 0; PreviewSymbol = createSymbol(); EndLocation = 0; Preview = text "not created" } }