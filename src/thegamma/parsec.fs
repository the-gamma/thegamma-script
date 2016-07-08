// ------------------------------------------------------------------------------------------------
// Minimal parser combinator library with a few optimized functions
// that should hopefuly make it work decently when compiled to JavaScript
// ------------------------------------------------------------------------------------------------

module TheGamma.Parsec

/// A parser takes a list of inputs and either fails or produces a list
/// of unconsumed inputs together with the result of the parsing
type Parser<'T, 'R> = Parser of (list<'T> -> option<list<'T> * 'R>)

type ParserSetter<'T, 'R> = 
  { Set : Parser<'T, 'R> -> unit }

let slot () = 
  let mutable slot = None
  { Set = fun (Parser p) -> slot <- Some p },
  Parser(fun input -> slot.Value input)

/// Creates a delayed parser to allow recursive parser definitions
let delay f = Parser(fun input -> 
  let (Parser g) = f ()
  g input )
  
/// If the input matches the specified prefix, produce the specified result
let prefix (prefix:list<'C>) result = Parser(fun input ->
  let rec loop (word:list<'C>) input =
    match word, input with
    | c::word, i::input when c = i -> loop word input
    | [], input -> Some(input)
    | _ -> None

  match loop prefix input with
  | Some(input) -> Some(input, result)
  | _ -> None)

/// Parser that succeeds when either of the two arguments succeed
let (<|>) (Parser p1) (Parser p2) = Parser(fun input ->
  match p1 input with
  | Some(input, res) -> Some(input, res)
  | _ -> p2 input)

/// Run two parsers in sequence and return the result as a tuple
let (<*>) (Parser p1) (Parser p2) = Parser(fun input ->
  match p1 input with
  | Some(input, res1) ->
      match p2 input with
      | Some(input, res2) -> Some(input, (res1, res2))
      | _ -> None
  | _ -> None)

/// Transforms the result of the parser using the specified function
let map f (Parser p) = Parser(fun input -> 
  p input |> Option.map (fun (input, res) -> input, f res))

/// Parser that tries to use a specified parser, but returns None if it fails
let optional (Parser p) = Parser(fun input ->
  match p input with
  | None -> Some(input, None)
  | Some(input, res) -> Some(input, Some res) )

/// Parser that succeeds if the input matches a predicate
let pred p = Parser(function
  | c::input when p c -> Some(input, c)
  | _ -> None)

/// Parser that succeeds if the predicate returns Some value
let choose p = Parser(function
  | c::input -> p c |> Option.map (fun c -> input, c)
  | _ -> None)

/// Parse zero or more repetitions using the specified parser
let zeroOrMore (Parser p) = 
  let rec loop acc input = 
    match p input with
    | Some(input, res) -> loop (res::acc) input
    | _ -> Some(input, List.rev acc)
  Parser(loop [])     

/// Parse one or more repetitions using the specified parser
let oneOrMore p = 
  (p <*> (zeroOrMore p)) 
  |> map (fun (c, cs) -> c::cs)

/// Parse a sequence of inputs recognized using the secified sequence
/// of parsers. Calling `sequenceChoices [ p1 .. pn ]` is a like
/// `zeroOrMore (p1 <|> .. <|> pn)` but faster and non-recursive.
let sequenceChoices parsers = Parser(fun input ->
  let mutable input = input
  let mutable running = true
  let mutable results = []
  while running do
    running <- false
    let mutable parsers = parsers
    while not (List.isEmpty parsers) do
      let (Parser p) = List.head parsers
      match p input with 
      | Some(newInput, res) ->
          input <- newInput
          running <- true
          results <- res::results
          parsers <- []
      | None -> 
          parsers <- List.tail parsers
  Some(input, List.rev results))

