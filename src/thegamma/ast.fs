// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

namespace TheGamma
open Fable.Extensions

type Range = 
  { Start : int
    End : int }

type Error<'Range> =
  { Number : int
    Message : string
    Range : 'Range }

type [<RequireQualifiedAccess>] Operator = 
  | Plus
  | Minus
  | Multiply
  | Divide
  | GreaterThan
  | LessThan
  | GreaterThanOrEqual
  | LessThanOrEqual

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

type Token = 
  { Token : TokenKind 
    Range : Range }

type Name = 
  { Name : string
    Range : Range }

type Argument<'T> = 
  { Name : Name option
    Value : Expr<'T> }

and Command<'T> = 
  { Command : CommandKind<'T>
    Range : Range }

and CommandKind<'T> = 
  | Let of Name * Expr<'T>
  | Expr of Expr<'T>

and [<RequireQualifiedAccess>] ExprKind<'T> = 
  | Variable of Name
  | Property of Expr<'T> * Name
  | Call of Expr<'T> * Name * Argument<'T> list
  | Function of Name * Expr<'T>
  | String of string
  | Number of float
  | Boolean of bool
  | List of Expr<'T> list
  | Empty
  | Unit
  | Null

and Expr<'T> =
  { Expr : ExprKind<'T>
    Range : Range 
    Type : 'T }

type Program<'T> = 
  { Body : Command<'T> list 
    Range : Range }


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


module Ranges = 
  let unionRanges r1 r2 =
    { Start = min r1.Start r2.Start; End = max r1.End r2.End }
  let strictSubRange first second = 
    (first.Start > second.Start && first.End <= second.End) ||
    (first.Start >= second.Start && first.End < second.End)
