namespace TheGamma

/// Represents range in 1-based line / 1-based column format
type LineColumnRange =
  { StartLineNumber : int
    StartColumn : int
    EndLineNumber : int
    EndColumn : int }

type LocationMapper(code:string) = 
  let lengths = code.Split('\n') |> Array.map (fun s -> s.Length)

  /// Convert absolute 0-based location to 1-based line and 1-based column location
  member x.AbsoluteToLineCol(offs) = 
    let mutable line = 0
    let mutable col = 0
    let mutable offs = offs
    while line <= lengths.Length && offs > lengths.[line] do
      offs <- offs - lengths.[line] - 1
      line <- line + 1
    line + 1, offs + 1

  /// Convert 1-based line and 1-based column location to an absolute 0-based location
  member x.LineColToAbsolute(line, col) = 
    let mutable offs = 0
    for l in 1 .. line-1 do offs <- offs + lengths.[l-1] + 1
    offs + col - 1

