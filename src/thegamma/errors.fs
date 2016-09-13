module TheGamma.Errors
open TheGamma.Ast

module Tokenizer = 
  let inputEndInsideString rng s =
    { Number = 101; Range = rng; Message = sprintf "Missing \" at the end of the input. The string \"%s\" ends without closing double-quote." s }
  let missingClosingQuote rng q = 
    { Number = 102; Range = rng; Message = sprintf "Quoted identifier '%s' is missing closing quote." q }
  let unexpectedCharacter rng (c:char) =
    { Number = 103; Range = rng; Message = sprintf "Unexcpected character '%s' in the input." (string c) }

module Parser = 
  let unexpectedTokenAfterDot rng tok =
    { Number = 201; Range = rng; Message = sprintf "Unexpected %s after '.' in method chain" (formatTokenInfo tok) }

  let unexpectedScopeEndAfterDot rng chainRng tok =
    { Number = 202; Range = rng; Message = sprintf "Unexpected end of scope after '.' in method chain before %s" (formatTokenInfo tok) }

  let unindentedIdentifierAfterDot rng chainRng id =
    { Number = 203; Range = rng; Message = sprintf "Unexpected end of scope after '.' and before '%s'. Indent the identifier?" id }

  let unindentedDotAfterIdentifier rng dotRng =
    { Number = 204; Range = rng; Message = "Dot after this identifier is not correctly nested" }

  let unindentedBlock rng tok =
    { Number = 205; Range = rng; Message = sprintf "Token following %s needs to be indented further" (formatTokenInfo tok) }

  let unexpectedTokenAfterOperator rng op tok =
    { Number = 206; Range = rng; Message = sprintf "Unexpected token '%s' after operator '%s'" (formatTokenInfo tok) (formatTokenInfo op) }

  let unexpectedTokenInArgList rng tok =
    { Number = 207; Range = rng; Message = sprintf "Unexpected token '%s' in list of call arguments" (formatTokenInfo tok) }

  let unexpectedScopeEndInArgList rng =
    { Number = 208; Range = rng; Message = "Unexpected end of scope in argument list" }

  let unexpectedTokenInParenthesizedExpr rng tok =
    { Number = 209; Range = rng; Message = sprintf "Unexpected token '%s' in parenthesized expression. Are you missing ')'?" (formatTokenInfo tok) }

  let unindentedTokenInParenthesizedExpr rng =
    { Number = 210; Range = rng; Message = "Unexpected end of nested expression in `(`" }

  let missingParenthesizedExpr rng =
    { Number = 211; Range = rng; Message = "The parenthesized expression (...) is missing body!" }

  let unexpectedTokenInList rng tok =
    { Number = 212; Range = rng; Message = sprintf "Unexpected token '%s' in list expression" (formatTokenInfo tok) }

  let unexpectedScopeEndInList rng =
    { Number = 213; Range = rng; Message = "Unexpected end of scope in list expression" }

  let unexpectedTokenInLetBinding rng tok =
    { Number = 214; Range = rng; Message = sprintf "Unexpected token '%s' in let declaration (should be let name = expr)" (formatTokenInfo tok) }

  let missingBodyInLetBinding rng =
    { Number = 215; Range = rng; Message = "This let binding is missing body after equals" }

  let nestedExpressionInCommand rng =
    { Number = 216; Range = rng; Message = "Unexpected expression" }

  let exceptionWhileParsing rng msg = 
    { Number = 299; Range = rng; Message = "Unexpected exception while parsing: " + msg }


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
