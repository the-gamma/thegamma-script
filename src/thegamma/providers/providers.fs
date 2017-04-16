namespace TheGamma.TypeProviders

open TheGamma
open TheGamma.Babel
open TheGamma.Common
open Fable.Import

type ProvidedType = 
  | NamedType of name:string * typ:Type
  | GlobalValue of string * Metadata list * Expression * Type

module ProviderHelpers = 
  let docMeta doc = 
    { Context = "http://thegamma.net"
      Type = "Documentation"
      Data = box doc }
