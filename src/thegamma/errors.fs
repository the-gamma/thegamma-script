module TheGamma.Errors

module Tokenizer = 
  let missingClosingQuote rng q = 
    { Number = 12; Range = rng; Message = sprintf "Quoted identifier '%s' is missing closing quote." q }

module Parser = 
  let valueNotAfunction rng name =
    { Number = 21; Range = rng 
      Message = sprintf "Global value '%s' is not a function. Ignoring arguments." name }

  let emptyIdentifier rng =
    { Number = 22; Range = rng; Message = "Empty identifier" }

  let missingClosingParen rng =
    { Number = 23; Range = rng; Message = "Missing closing )" }

  let nothingAfterDot rng =
    { Number = 24; Range = rng; Message = "Call chain should not end with ." }

module TypeChecker = 
  let private formatMembers members = 
    [ for m in members -> match m with Member.Method(n, _, _) -> n | Member.Property(n, _) -> n ] 
    |> String.concat ", " 

  let variableNotInScope rng name =
    { Number = 31; Range = rng 
      Message = sprintf "Variable '%s' is not in scope." name }
  
  let propertyMissing rng name members = 
    { Number = 32; Range = rng 
      Message = sprintf "Could not find property '%s' in the list '%s'." name (formatMembers members) }
  
  let methodMissing rng name members = 
    { Number = 33; Range = rng 
      Message = sprintf "Could not find method '%s' in the list '%s'." name (formatMembers members) }

  let notAnObject rng typ = 
    { Number = 34; Range = rng 
      Message = sprintf "Type is not an object." }
    

