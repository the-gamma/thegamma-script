namespace TheGamma.General

open System
open TheGamma
open TheGamma.Html
open TheGamma.Common
open Fable.Import.Browser
open Fable.Core

type math = 
  static member sqrt(f:float) = sqrt f
  static member pow(f:float, k) = Math.Pow(f, k)
  static member log(f:float, ?b) = match b with Some b -> Math.Log(f, b) | _ -> log f
  static member min(f1:float, f2:float) = min f1 f2
  static member max(f1:float, f2:float) = max f1 f2
  static member round(n:float, decimals:int) = Math.Round(n, decimals)
  static member ceil(n:float) = Math.Ceiling(n)
  static member floor(n:float) = Math.Floor(n)

type date = 
  static member now() = DateTime.Now
  static member date(year,month,day) = DateTime(year,month,day)
  static member time(year,month,day,hour,minute,second) = DateTime(year,month,day,hour,minute,second)

type dateformat = 
  static member longDate(dt:obj) : string = formatLongDate (unbox<System.DateTime> dt)
  
type number = 
  static member format(n:float, ?decimals) = niceNumber n (defaultArg decimals 999) 

type pair<'t1, 't2> internal (v1:'t1, v2:'t2) = 
  member x.first = v1
  member x.second = v2