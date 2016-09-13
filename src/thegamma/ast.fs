namespace TheGamma
open TheGamma.Common

// ------------------------------------------------------------------------------------------------
// Tokens and common 
// ------------------------------------------------------------------------------------------------

/// Represents a range as character offset in a file. 0-indexed, the End position is the position
/// of the last character of the thing in the range. Consider range of "42" in "1 + 42 + 2".
/// The range of the token 42 here would be { Start = 4; End = 5 }:
///
///     1   +   4 2   +   2 
///     0 1 2 3 4 5 6 7 8 9
///
type Range = 
  { Start : int
    End : int }

/// Error with a range. Message can be Markdown, Range is generic so that we can reuse 
/// the data structure with both `Range` and line-based range when reporting errors.
type Error<'Range> =
  { Number : int
    Message : string
    Range : 'Range }

/// Binary operators (Equals is tokenized as separate token, but after parsing it can be operator)
type [<RequireQualifiedAccess>] Operator = 
  | Equals
  | Plus
  | Minus
  | Multiply
  | Divide
  | Power
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual

/// Tokens produced by tokenizer
type [<RequireQualifiedAccess>] TokenKind = 
  | LParen
  | RParen
  | Equals
  | Dot
  | Comma
  | Let
  | LSquare
  | RSquare
  | Fun
  | Arrow
  | To
  | By
  | Operator of Operator
  | Boolean of bool
  | Number of string * float
  | String of string
  | Ident of string
  | QIdent of string
  | White of string
  | Newline
  | Error of char
  | EndOfFile

/// Token with a range
type Token = 
  { Token : TokenKind 
    Range : Range }


// ------------------------------------------------------------------------------------------------
// Parsed AST 
// ------------------------------------------------------------------------------------------------

/// Node wraps syntax element with other information. Whitespce before/after are tokens 
/// around it that the parser skipped (they may be whitespace, but also skipped error tokens)
type Node<'T> = 
  { WhiteBefore : Token list
    WhiteAfter : Token list
    Range : Range 
    Node : 'T }

/// Name, usually appears as Node<Name> 
type Name = 
  { Name : string }

/// Method call argument, optionally with a named
type Argument =
  { Name : Node<Name> option
    Value : Node<Expr> }

/// A program is a list of commands (with range info)
and Program = 
  { Body : Node<Node<Command> list> }

/// Variable binding or an expression
and Command = 
  | Let of Node<Name> * Node<Expr>
  | Expr of Node<Expr>

/// An expression (does not include let binding, which is a command)
and [<RequireQualifiedAccess>] Expr = 
  | Variable of Node<Name>
  | Property of Node<Expr> * Node<Name>
  | Call of Node<Expr> option * Node<Name> * Node<Argument list>
  | Function of Node<Name> * Node<Expr>
  | String of string
  | Number of float
  | Boolean of bool
  | Binary of Node<Expr> * Node<Operator> * Node<Expr>
  | List of Node<Expr> list
  | Empty
  | Unit
  | Null

// ------------------------------------------------------------------------------------------------
// Types and code generation
// ------------------------------------------------------------------------------------------------

type Emitter = 
  { Emit : Babel.Expression * (string * Babel.Expression) list -> Babel.Expression }

type Schema = 
  { Type : string; JSON : obj }

type [<RequireQualifiedAccess>] Documentation = 
  | Text of string
  | Details of string * string
  | None 

type [<RequireQualifiedAccess>] Member = 
  | Property of name:string * typ:Type * schema:Schema option * docs:Documentation * emitter:Emitter
  | Method of name:string * typars:string list * arguments:(string * bool * Type) list * typ:Type * docs:Documentation * emitter:Emitter
  member x.Name = 
    match x with Property(name=s) | Method(name=s) -> s

and ObjectType = 
  { Members : Member[] 
    Typeargs : Type list }

and [<RequireQualifiedAccess>] Type =
  | Delayed of guid:string * Future<Type>
  | Primitive of string
  | Object of ObjectType
  | Function of arguments:Type list * returns:Type
  | Parameter of string
  | List of elementType:Type
  | Unit
  | Any
