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
  | Colon
  | Fun
  | Arrow
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
// Types and code generation
// ------------------------------------------------------------------------------------------------

type Emitter = 
  { Emit : Babel.Expression * Babel.Expression list -> Babel.Expression }

type Metadata = 
  { Context : string
    Type : string
    Data : obj }

type [<RequireQualifiedAccess>] Documentation = 
  | Text of string
  | Details of string * string
  | None 

type Member = 
  { Name : string 
    Type : Type 
    Metadata : Metadata list 
    Emitter : Emitter }

and ObjectType = 
  abstract Members : Member[] 
  abstract TypeEquals : ObjectType -> bool

and [<RequireQualifiedAccess>] PrimitiveType = 
  | Number
  | Date
  | String
  | Bool
  | Unit

and [<RequireQualifiedAccess>] Type =
  | Delayed of Future<Type>
  | Object of ObjectType
  | Primitive of PrimitiveType
  | List of elementType:Type
  | Method of arguments:(string * bool * Type) list * typ:(Type list -> Type option) 
  | Any

// ------------------------------------------------------------------------------------------------
// Entities - binder attaches those to individual constructs in the parsed AST
// ------------------------------------------------------------------------------------------------

/// Name. In expressions, it usually appears as Node<Name> 
type Name = 
  { Name : string }

/// Represents constants that can appear in the code
/// (We create separate entity for each, so that we can calculate
/// values of entities and not just types)
type [<RequireQualifiedAccess>] Constant = 
  | Number of float
  | String of string
  | Boolean of bool
  | Empty

/// Represents different kinds of entities that we create. Roughhly
/// corresponds to all places in code where something has a name.
type [<RequireQualifiedAccess>] EntityKind = 

  // Entities that represent root node, program and commands
  | Root
  | Program of commands:Entity list
  | RunCommand of body:Entity
  | LetCommand of variable:Entity * assignment:Entity

  // Standard constructs of the language
  | List of elements:Entity list
  | Constant of Constant
  | Function of variable:Entity * body:Entity
  | Operator of left:Entity * operator:Operator * right:Entity

  /// Reference to a global symbol or a local variable
  | GlobalValue of name:Name * Babel.Expression option 
  | Variable of name:Name * value:Entity

  /// Variable binding in lambda abstraction
  | Binding of name:Name * callSite:Entity
  /// Call site in which a lambda function appears. Marks method reference & argument
  /// (the argument is the name or the index of the parameter in the list)
  | CallSite of instance:Entity * parameter:Choice<string, int>

  /// Represents all arguments passed to method; Antecedants are individual arguments
  /// (a mix of named parameter & ordinary expression entities)
  | ArgumentList of arguments:Entity list
  /// Named param in a call site with an expression assigned to it
  | NamedParam of name:Name * assignment:Entity

  /// Placeholder with its name and the body entity
  | Placeholder of name:Name * body:Entity

  /// Member access and call with arguments (call has member access 
  /// as the instance; second argument of Member is MemberName)
  | Call of instance:Entity * arguments:Entity
  | Member of instance:Entity * name:Entity
  | MemberName of name:Name


  
/// An entity represents a thing in the source code to which we attach additional info.
/// It is uniquely identified by its `Symbol` (which is also used for lookups)
and Entity = 
  { Kind : EntityKind
    Symbol : Symbol 
    mutable Value : EntityValue option
    mutable Meta : Metadata list
    mutable Type : Type option 
    mutable Errors : Error<Range> list }

and RuntimeValue = interface end

and EntityValue =
  { Value : RuntimeValue
    Preview : Lazy<RuntimeValue option> }

// ------------------------------------------------------------------------------------------------
// Parsed AST 
// ------------------------------------------------------------------------------------------------

/// Node wraps syntax element with other information. Whitespce before/after are tokens 
/// around it that the parser skipped (they may be whitespace, but also skipped error tokens).
/// Entity is assigned to the expression later by a binder.
type Node<'T> = 
  { WhiteBefore : Token list
    WhiteAfter : Token list
    Range : Range 
    Node : 'T
    mutable Entity : Entity option }

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
  | Member of Node<Expr> * Node<Expr>
  | Call of Node<Expr> * Node<Argument list>
  | Function of Node<Name> * Node<Expr>
  | Placeholder of Node<Name> * Node<Expr>
  | String of string
  | Number of float
  | Boolean of bool
  | Binary of Node<Expr> * Node<Operator> * Node<Expr>
  | List of Node<Expr> list
  | Empty

