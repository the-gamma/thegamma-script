// ------------------------------------------------------------------------------------------------
// 
// ------------------------------------------------------------------------------------------------

namespace TheGamma

type Location = 
  { Line : int
    Column : int }

type Range = 
  { Start : Location
    End : Location }

[<RequireQualifiedAccess>]
type TokenKind = 
  | LParen
  | RParen
  | Equals
  | Dot
  | Comma
  | Let
  | Operator of string
  | Boolean of bool
  | Number of string * float
  | Ident of string
  | QIdent of string
  | White of string

type Token = 
  { Token : TokenKind 
    Range : Range }

type Name = string

type Argument<'T> = 
  { Name : Name option
    Value : Expr<'T> }

and ExprKind<'T> = 
  | Variable of Name
  | Property of Expr<'T> * Name
  | Call of Expr<'T> * Name * Argument<'T> list
  | Number of float
  | Boolean of bool

and Expr<'T> =
  { Expr : ExprKind<'T>
    Range : Range 
    Type : 'T }

type Future<'T> = 
  abstract Then : ('T -> unit) -> unit

type Member = 
  | Property of string * Type
  | Method of string * (string * Type) list * Type

and ObjectType = 
  { Members : Member list }

and Type =
  | Delayed of Future<Type>
  | Primitive of string
  | Object of ObjectType