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

let updateBody trigger state = 
  Log.trace("live", "Showable - updating body")
  match commandAtLocation state.Location state.Program with
  | Some({ Node = Command.Expr e; Entity = Some { Kind = EntityKind.RunCommand ent } } as cmd) ->
      match ent.Type.Value with 
      | Type.Object obj ->
          let hasShow = obj.Members |> Array.exists (function 
            | { Name="show"; Type=Type.Method([{ Type = Type.Primitive PrimitiveType.String }], _) } -> true
            | _ -> false)
          if hasShow then
            let res = Interpreter.evaluate state.Globals ent        
            match res with 
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
            else None
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
  { Update = updateShowableState
    Render = renderShowable
    InitialState = { PreviewID = 0; PreviewSymbol = createSymbol(); EndLocation = 0; Preview = text "not created" } }