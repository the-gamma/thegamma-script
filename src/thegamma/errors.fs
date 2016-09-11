module TheGamma.Errors
open TheGamma.AstOperations

module Tokenizer = 
  let inputEndInsideString rng s =
    { Number = 11; Range = rng; Message = sprintf "Missing \" at the end of the input. The string \"%s\" ends without closing double-quote." s }
  let missingClosingQuote rng q = 
    { Number = 12; Range = rng; Message = sprintf "Quoted identifier '%s' is missing closing quote." q }
  let unexpectedCharacter rng (c:char) =
    { Number = 13; Range = rng; Message = sprintf "Unexcpected character '%s' in the input." (string c) }

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

  let nothingAfterComma rng =
    { Number = 25; Range = rng; Message = "List should not end with ," }

  let missingClosingSquare rng =
    { Number = 26; Range = rng; Message = "Missing closing ]" }

  let incompleteRange rng =
    { Number = 27; Range = rng; Message = "Incomeplete range expression" }

  // v2 errors

  let unexpectedTokenAfterDot rng tok =
    { Number = 21; Range = rng; Message = sprintf "Unexpected %s after '.' in method chain" (formatTokenInfo tok) }

  let unexpectedScopeEndAfterDot rng chainRng tok =
    { Number = 22; Range = rng; Message = sprintf "Unexpected end of scope after '.' in method chain before %s" (formatTokenInfo tok) }

  let unindentedIdentifierAfterDot rng chainRng id =
    { Number = 23; Range = rng; Message = sprintf "Unexpected end of scope after '.' and before '%s'. Indent the identifier?" id }


module TypeChecker = 
  let private formatMembers members = 
    [ for Member.Method(name=n) | Member.Property(name=n) in members -> n ] 
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

  let mismatchingListTypes rng = 
    { Number = 33; Range = rng 
      Message = "The types of list elements do not match." }

  let notAnObject rng typ = 
    { Number = 34; Range = rng 
      Message = "Type is not an object." }
    
  let cannotUnityTypes rng = 
    { Number = 35; Range = rng 
      Message = "Cannot unify types." }

  let nameBasedParamMustBeLast rng = 
    { Number = 36; Range = rng 
      Message = "All named parameters must be at the end of parameter list." }
