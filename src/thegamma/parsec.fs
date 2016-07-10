// ------------------------------------------------------------------------------------------------
// Minimal parser combinator library with a few optimized functions
// that should hopefuly make it work decently when compiled to JavaScript
// ------------------------------------------------------------------------------------------------

module TheGamma.Parsec

type ParseStream<'T> = int * list<'T>

/// A parser takes a list of inputs and either fails or produces a list
/// of unconsumed inputs together with the result of the parsing
type Parser<'T, 'R> = Parser of (ParseStream<'T> -> option<ParseStream<'T> * Error list * 'R>)

/// Returned by the `slot` function to create a parser slot that is filled later
type ParserSetter<'T, 'R> = 
  { Set : Parser<'T, 'R> -> unit }

/// Ignore the result of the parser
let ignore (Parser p) = Parser(fun input -> 
  p input |> Option.map (fun (i, e, r) -> i, e, ()))

/// Creates a delayed parser whose actual parser is set later
let slot () = 
  let mutable slot = None
  { Set = fun (Parser p) -> slot <- Some p },
  Parser(fun input -> slot.Value input)

/// If the input matches the specified prefix, produce the specified result
let prefix (prefix:list<'C>) result = Parser(fun (offset, input) ->
  let rec loop (word:list<'C>) input =
    match word, input with
    | c::word, i::input when c = i -> loop word input
    | [], input -> Some(input)
    | _ -> None

  match loop prefix input with
  | Some(input) -> Some((offset+List.length prefix, input), [], result)
  | _ -> None)

/// Parser that succeeds when either of the two arguments succeed
let (<|>) (Parser p1) (Parser p2) = Parser(fun input ->
  match p1 input with
  | Some(input, err, res) -> Some(input, err, res)
  | _ -> p2 input)

/// Run two parsers in sequence and return the result as a tuple
let (<*>) (Parser p1) (Parser p2) = Parser(fun input ->
  match p1 input with
  | Some(input, e1, res1) ->
      match p2 input with
      | Some(input, e2, res2) -> Some(input, e1 @ e2, (res1, res2))
      | _ -> None
  | _ -> None)

/// Transforms the result of the parser using the specified function
let map f (Parser p) = Parser(fun input -> 
  p input |> Option.map (fun (input, err, res) -> input, err, f res))

/// Run two parsers in sequence and return the result of the second one
let (<*>>) p1 p2 = p1 <*> p2 |> map snd

/// Run two parsers in sequence and return the result of the first one
let (<<*>) p1 p2 = p1 <*> p2 |> map fst

/// Succeed without consuming input
let unit res = Parser(fun input -> Some(input, [], res))

/// Report an error and succeed without consuming input
let error e = Parser(fun input -> Some(input, [e], ()))

/// Parse using the first parser and then call a function to produce
/// next parser and parse the rest of the input with the next parser
let bind f (Parser p) = Parser(fun input ->
  match p input with
  | Some(input, err1, res) ->
      let (Parser g) = f res
      match g input with
      | Some(input, err2, res) -> Some(input, err1 @ err2, res)
      | _ -> None
  | _ -> None)       
  
/// Parser that tries to use a specified parser, but returns None if it fails
let optional (Parser p) = Parser(fun input ->
  match p input with
  | None -> Some(input, [], None)
  | Some(input, err, res) -> Some(input, err, Some res) )

/// Parser that succeeds if the input matches a predicate
let pred p = Parser(function
  | offs, c::input when p c -> Some((offs+1, input), [], c)
  | _ -> None)

/// Parser that succeeds if the predicate returns Some value
let choose p = Parser(function
  | offs, c::input -> p c |> Option.map (fun c -> (offs + 1, input), [], c)
  | _ -> None)

/// Parse zero or more repetitions using the specified parser
let zeroOrMore (Parser p) = 
  let rec loop acc errs input = 
    match p input with
    | Some(input, e, res) -> loop (res::acc) (e @ errs) input
    | _ -> Some(input, errs, List.rev acc)
  Parser(loop [] [])     

/// Parse one or more repetitions using the specified parser
let oneOrMore p = 
  (p <*> (zeroOrMore p)) 
  |> map (fun (c, cs) -> c::cs)

/// Run the specified parser and annotate the result with range information
let range (Parser p) = Parser(fun ((offs1, _) as input) ->
  p input |> Option.map (fun (((offs2, _) as input), errs, res) -> 
    input, errs, ({ Start = offs1; End = offs2 }, res) ))

/// Parse a sequence of inputs recognized using the secified sequence
/// of parsers. Calling `sequenceChoices [ p1 .. pn ]` is a like
/// `zeroOrMore (p1 <|> .. <|> pn)` but faster and non-recursive.
let sequenceChoices parsers = Parser(fun input ->
  let mutable input = input
  let mutable running = true
  let mutable results = []
  let mutable errors = []
  while running do
    running <- false
    let mutable parsers = parsers
    while not (List.isEmpty parsers) do
      let (Parser p) = List.head parsers
      match p input with 
      | Some(newInput, err, res) ->
          input <- newInput
          running <- true
          results <- res::results
          parsers <- []
          errors <- err @ errors
      | None -> 
          parsers <- List.tail parsers
  Some(input, errors, List.rev results))

