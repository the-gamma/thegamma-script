module TheGamma.Live.Common

open TheGamma
open Fable.Import

// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

type CustomLiveState = interface end
type CustomLiveEvent = interface end
type LiveEditorZone = { Line:int; Preview:Html.DomNode }

type LivePreview<'TState, 'TEvent> =
  { Update : (LiveEvent<'TEvent> -> unit) -> LiveState<'TState> -> LiveEvent<'TEvent> -> LiveState<'TState> option
    Render : (LiveEvent<'TEvent> -> unit) -> LiveState<'TState> -> LiveEditorZone option
    InitialState : 'TState }

and LiveState<'T> =
  { // Initialized once - global values
    Globals : seq<Entity>
    // Updated when code changes - parsed program
    Code : string
    Program : Program
    Mapper : LocationMapper
    // Updated when cursor moves 
    Location : int
    // Instructing the event loop to do things to the editor  
    Selection : option<LineColumnRange>
    
    State : 'T
    CurrentPreview : option<LivePreview<CustomLiveState, CustomLiveEvent>> }

and LiveEvent<'T> =
  | InitializeGlobals of seq<Entity>
  | UpdateSource of string * int * Program * LocationMapper
  | UpdateLocation of int
  | CustomEvent of 'T

let updateLiveState state event = 
  match event with
  | InitializeGlobals(globals) ->
      { state with Globals = globals }
  | UpdateLocation(loc) ->
      { state with Location = loc }
  | UpdateSource(code, loc, program, mapper) ->
      { state with Location = loc; Program = program; Code = code; Mapper = mapper }
  | CustomEvent _ -> state
