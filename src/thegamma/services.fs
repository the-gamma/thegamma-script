module TheGamma.Services

open Fable.Import

open TheGamma.Html
open TheGamma.Editors
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// Editors
// ------------------------------------------------------------------------------------------------

type EditorWorkerMessage = 
  | Update of string
  | UpdateNow of string
  | Refersh of string

type EditorService(article, checker:string -> Async<bool * Program>, delay) = 
  let renderEditors = Control.Event<_>()
  let update text = async {
    Log.event("options", "update", article, text)
    let! (_:bool), prg = checker text 
        
    Log.trace("service", "Collecting editors")
    //let! eds = Async.collect collectCmdEditors prg.Body 
    let eds = []
    let eds = eds |> List.mapi (fun i v -> i, v)
    let filteredEds = 
      eds 
      |> List.filter (fun (i, ed1) ->
          eds |> List.exists (fun (j, ed2) -> j <> i && Ast.strictSubRange ed1.Range ed2.Range) |> not)
      |> List.map snd

    Log.trace("service", "Rendering %s out of %s", filteredEds.Length, eds.Length)
    renderEditors.Trigger filteredEds }

  let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop lastText pending = async {
      let! msg = inbox.Receive()
      match msg with
      | Update text -> 
          async { do! Async.Sleep(delay)
                  inbox.Post(Refersh text) } |> Async.StartImmediate
          return! loop lastText (pending+1)
      | UpdateNow text ->
          try
            Log.trace("editors", "updating...")
            if text <> lastText then do! update text
          with e -> 
            Log.exn("editors", "update failed: %O", e)
          return! loop text pending
      | Refersh text ->
          if pending = 1 then 
            try
              Log.trace("editors", "updating...")
              if text <> lastText then do! update text
            with e -> 
              Log.exn("editors", "update failed: %O", e)
          return! loop text (pending-1) }
    loop "" 0)

  member x.UpdateSource(text, ?immediately) = 
    if immediately = Some true then agent.Post(UpdateNow text)
    else agent.Post(Update text)
  member x.EditorsUpdated = renderEditors.Publish


// ------------------------------------------------------------------------------------------------
// Type checker
// ------------------------------------------------------------------------------------------------

type CheckingMessage = 
  | TypeCheck of code:string * AsyncReplyChannel<bool * Program>
  | IsWellTyped of code:string * AsyncReplyChannel<bool>

type Position = { Line:int; Column:int }
type LineRange = { Start:Position; End:Position }

let rec offsetToLocation lines offs lengths =
  match lengths with
  | l::lengths when offs <= l -> { Line = lines; Column = offs }
  | l::lengths -> offsetToLocation (lines+1) (offs-l-1) lengths
  | [] -> { Line = lines; Column = offs  } // error? out of range

let rangeToLoc lengths (rng:Range) = 
  { Start = offsetToLocation 1 rng.Start lengths
    End = offsetToLocation 1 rng.Start lengths }

type CheckingService(article, globals:Future<Map<string, Type>>) =
  let errorsReported = Control.Event<_>()
  let emptyProg = { Body = Ast.node { Start = 0; End = 0 } [] }
  let agent = MailboxProcessor.Start(fun inbox ->
    let rec loop lastCode lastResult = async {
      let! msg = inbox.Receive()
      match msg with
      | IsWellTyped(code, repl) ->
          let! globals = Async.AwaitFuture globals
          try
            //let! errors, result = TypeChecker.typeCheck globals code
            //repl.Reply(errors.IsEmpty)
            repl.Reply(true)
          with e ->
            Log.exn("service", "Type checking failed: %O", e)
            repl.Reply(false)
          return! loop lastCode lastResult

      | TypeCheck(code, repl) when code = lastCode ->
          Log.trace("service", "Returning previous result")
          repl.Reply(lastResult)
          return! loop lastCode lastResult

      | TypeCheck(code, repl) ->
          Log.trace("service", "Type checking source code")
          let! globals = Async.AwaitFuture globals
          try
            Log.event("compiler", "check-source", article, code)
            let result, errors = Parser.parseProgram code
            //let! errors, result = TypeChecker.typeCheck globals code
            Log.trace("service", "Type checking completed")

            let lengths = code.Split('\n') |> Array.toList |> List.map (fun l -> l.Length)
            let errors = errors |> Array.map (fun e -> 
              { Number = e.Number; Message = e.Message; Range = rangeToLoc lengths e.Range })

            errorsReported.Trigger(code, errors)
            let result = (errors.Length = 0, result)
            repl.Reply(result) 
            return! loop code result
          with e ->
            Log.exn("service", "Type checking failed: %O", e)
            repl.Reply((false, emptyProg))
            return! loop lastCode lastResult }
    loop "" (false, emptyProg))

  member x.ErrorsReported = errorsReported.Publish
  member x.TypeCheck(code) = agent.PostAndAsyncReply(fun ch -> TypeCheck(code, ch))
  member x.IsWellTyped(code) = agent.PostAndAsyncReply(fun ch -> IsWellTyped(code, ch))

