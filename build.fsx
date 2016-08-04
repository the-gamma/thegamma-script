// --------------------------------------------------------------------------------------
// A simple FAKE build script that:
//  1) Hosts Suave server locally & reloads web part that is defined in 'app.fsx'
//  2) Deploys the web application to Azure web sites when called with 'build deploy'
// --------------------------------------------------------------------------------------

#r "packages/Suave/lib/net40/Suave.dll"
#r "packages/FAKE/tools/FakeLib.dll"
open Fake

open System
open System.IO
open Suave
open Suave.Operators
open Suave.Web

// --------------------------------------------------------------------------------------
// Suave server 
// --------------------------------------------------------------------------------------

let server = 
  Writers.setHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  >=> Writers.setHeader "Pragma" "no-cache"
  >=> Writers.setHeader "Expires" "0"
  >=> choose [
    Files.browse (__SOURCE_DIRECTORY__ </> "web")
    Files.browse (__SOURCE_DIRECTORY__) ]

let config =
  { defaultConfig with
      logger = Logging.Loggers.saneDefaultsFor Logging.LogLevel.Debug
      bindings = [ HttpBinding.mkSimple HTTP  "127.0.0.1" 8899 ] }

Target "justrun" (fun _ ->
  let _, server = startWebServerAsync config server
  Async.Start(server)
  System.Diagnostics.Process.Start("http://localhost:8899/index.html") |> ignore
)

// --------------------------------------------------------------------------------------
//
// --------------------------------------------------------------------------------------

(*
let npm command args workingDir =
  let args = sprintf "%s %s" command (String.concat " " args)
  let cmd, args = if EnvironmentHelper.isUnix then "npm", args else "cmd", ("/C npm " + args)
  let ok =
    execProcess (fun info ->
      info.FileName <- cmd
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue
  if not ok then failwith (sprintf "'%s %s' task failed" cmd args)

let spawnNode command args workingDir =
  let args = sprintf "%s %s" command (String.concat " " args)
  let cmd, args = if EnvironmentHelper.isUnix then "node", args else "cmd", ("/C node " + args)
  async { 
    execProcess (fun info ->
      info.FileName <- cmd
      info.WorkingDirectory <- workingDir
      info.Arguments <- args) TimeSpan.MaxValue |> ignore } |> Async.Start

let fable = "../../paket-files/github.com/fsprojects/Fable/build/fable"

Target "justfable" (fun _ ->
  __SOURCE_DIRECTORY__ |> npm "install" []
  __SOURCE_DIRECTORY__ </> "src" </> "thegamma" |> spawnNode fable ["-w"]
  __SOURCE_DIRECTORY__ </> "src" </> "libraries" |> spawnNode fable ["-w"]
)
*)
let sleep _ = 
  traceImportant "Press any key to stop!"
  Console.ReadKey() |> ignore

Target "run" sleep
"justrun" ==> "run"

RunTargetOrDefault "run"
