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

  let unexpectedScopeEndAfterDot rng tok =
    { Number = 202; Range = rng; Message = sprintf "Unexpected end of scope after %s. Did you forget to indent the rest of the member chain?" (formatTokenInfo tok) }

  let unexpectedTokenInPlaceholder rng tok =
    { Number = 203; Range = rng; Message = sprintf "Unexpected token '%s' in placeholder expression. Shold be `[ident: <expr>]`" (formatTokenInfo tok) }

  let unexpectedScopeEndInPlaceholder rng tok =
    if Option.isSome tok then { Number = 204; Range = rng; Message = sprintf "Unexpected end of scope in placeholder after %s. Did you forget to indent the body of the plceholder?" (formatTokenInfo tok.Value) }
    else { Number = 204; Range = rng; Message = "Unexpected end of scope in placeholder after expression. Did you forget to indent the body of the plceholder?" }

  let unexpectedEndOfPlaceholder rng =
    { Number = 205; Range = rng; Message = "Incomplete placeholder expression. Shold be `[ident: <expr>]`" }

  let unexpectedEndAfterOperator rng op =
    { Number = 206; Range = rng; Message = sprintf "Unexpected token after operator '%s'. Expected an expression or closing parenthesis." (formatTokenInfo op) }

  let unexpectedTokenInArgList rng tok =
    { Number = 207; Range = rng; Message = sprintf "Unexpected token '%s' in list of call arguments" (formatTokenInfo tok) }

  let unexpectedScopeEndInArgList rng =
    { Number = 208; Range = rng; Message = "Unexpected end of argument list. Did you forget to indent the arguments?" }

  let unexpectedTokenInParenthesizedExpr rng tok =
    { Number = 209; Range = rng; Message = sprintf "Unexpected token '%s' in parenthesized expression. Are you missing ')'?" (formatTokenInfo tok) }

  let unexpectedScopeEndInParenthesizedExpr rng =
    { Number = 210; Range = rng; Message = "Unexpected end of nested expression in `(`. Did you forget to indent the body of the expression?" }

  let missingParenthesizedExpr rng =
    { Number = 211; Range = rng; Message = "The parenthesized expression (...) is missing body!" }

  let unexpectedTokenInList rng tok =
    { Number = 212; Range = rng; Message = sprintf "Unexpected token '%s' in list expression" (formatTokenInfo tok) }

  let unexpectedScopeEndInList rng =
    { Number = 213; Range = rng; Message = "Unexpected end of list expression. Did youu forget to indent the elements of the list?" }

  let unexpectedTokenInLetBinding rng tok =
    { Number = 214; Range = rng; Message = sprintf "Unexpected token '%s' in let declaration (should be `let name = expr`)" (formatTokenInfo tok) }

  let missingBodyInLetBinding rng =
    { Number = 215; Range = rng; Message = "This let binding is missing body after equals (should be `let name = expr`" }

  let unexpectedNestedTokenInCommand rng tok =
    { Number = 216; Range = rng; Message = sprintf "Unexpected indented token '%s' in command list. Try removing the indentation before the token." (formatTokenInfo tok) }

  let unexpectedTokenAfterFun rng tok =
    { Number = 217; Range = rng; Message = sprintf "Unexpected token '%s' after `fun`. Expected variable name." (formatTokenInfo tok) }

  let missingArrowInFunc rng =
    { Number = 218; Range = rng; Message = "Missing arrow after variable in function definition" }

  let unexpectedScopeEndInFunc rng =
    { Number = 219; Range = rng; Message = "Unexpected end of function declaration. Did you forget to indent the body of the function?" }

  let missingBodyOfFunc rng =
    { Number = 220; Range = rng; Message = "The function is missing body. Did you forget to indent the body of the function?" }

  let unexpectedScopeEndInLet rng =
    { Number = 221; Range = rng; Message = "Unexpected end of let declaration. Did you forget to indent the body of the let declaration?" }

  let exceptionWhileParsing rng msg = 
    { Number = 299; Range = rng; Message = "Unexpected exception while parsing: " + msg }

    
module TypeChecker = 
  let numericOperatorExpectsNumbers op idx typ rng =
    { Number = 301; Range = rng 
      Message = 
        sprintf "Both operands of binary operator '%s' should be numbers but the %s operand was %s instead." 
          (formatToken (TokenKind.Operator op)) (if idx = 0 then "left" else "right") (formatTypeInfo typ) }

  let variableNotInScope name rng =
    { Number = 302; Range = rng 
      Message = sprintf "Variable '%s' is not in scope." name }

  let private formatMembers (members:seq<Member>) = 
    [ for m in members -> m.Name ] |> String.concat ", " 

  let propertyMissing name members rng = 
    { Number = 303; Range = rng 
      Message = sprintf "Could not find property '%s' in the list '%s'." name (formatMembers members) }

  let methodMissing name members rng = 
    { Number = 304; Range = rng 
      Message = sprintf "Could not find method '%s' in the list '%s'." name (formatMembers members) }

  let notAnObject name typ rng = 
    { Number = 305; Range = rng 
      Message = sprintf "Type is not an object but %s and it does not have member `%s`" (formatTypeInfo typ) name }

  let listElementTypeDoesNotMatch listty elty rng = 
    { Number = 306; Range = rng 
      Message = sprintf "The type of this list element is %s but it should be %s" (formatTypeInfo elty) (formatTypeInfo listty) }

  let nameBasedParamMustBeLast rng = 
    { Number = 307; Range = rng 
      Message = "All named parameters must be at the end of parameter list." }

  let parameterMissingValue par rng = 
    { Number = 308; Range = rng 
      Message = sprintf "Required parameter `%s` is not given a value." par }

(*
  let incorrectParameterType parName parType actualType err1 err2 rng = 
    { Number = 309; Range = rng 
      Message = 
        sprintf "The value of parameter `%s` has wrong type. Expected %s but got %s. The type %s does not match the type %s."
          parName (formatTypeInfo parType) (formatTypeInfo actualType) (formatTypeInfo err1) (formatTypeInfo err2) }

  let inferenceConflict var t1 t2 rng = 
    { Number = 310; Range = rng 
      Message = 
        sprintf "The arguments of the call have conflicting types. The type %s assigned to a variable %s does not match the type %s."
          (formatTypeInfo t1) var (formatTypeInfo t2) }
*)
  let parameterConflict rng = 
    { Number = 308; Range = rng 
      Message = "Invalid argument type" }

  let callMissingInstance name rng = 
    { Number = 311; Range = rng 
      Message = sprintf "The `%s` property access or call is missing an instance" name }
