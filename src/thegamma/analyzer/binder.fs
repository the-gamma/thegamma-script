// ------------------------------------------------------------------------------------------------
// Binder attaches `Entity` objects to `Node` objects produced by the parser
// Entities are reused when possible and contain inferred types etc.
// ------------------------------------------------------------------------------------------------
module TheGamma.Binder

open TheGamma.Ast
open TheGamma.Common

/// As we bind, we keep root entity, current scope & variables in scope
type BindingContext = 
  { Variables : Map<Name, Entity>  
    Scope : Entity
    Root : Entity
    /// Table with previously created entities. This is a mutable mapping from 
    /// list of symbols (antecedent entities) together with entity kind & name
    /// to the actual entity. Antecedents capture dependencies (if dependency 
    /// changed, we need to recreate the entity that depends on them)
    Table : ListDictionary<Symbol, Map<EntityKind * Name, Entity>> 
    /// Collects all bound entities and their ranges
    Bound : ResizeArray<Range * Entity> }


/// Lookup entity (if it can be reused) or create & cache a new one
let bindEntity ctx kind antecedents name =
  let symbols = antecedents |> List.map (fun a -> a.Symbol)
  let nestedDict = 
    match ListDictionary.tryFind symbols ctx.Table with
    | None -> Map.empty
    | Some res -> res
  if nestedDict.ContainsKey (kind, name) then 
    Log.trace("binder", "Cached: binding %s %s", formatEntityKind kind, name.Name)
    nestedDict.[kind, name]
  else
    Log.trace("binder", "New: binding %s %s", formatEntityKind kind, name.Name)
    let symbol = createSymbol ()
    let entity = { Kind = kind; Antecedents = antecedents; Name = name; Symbol = symbol; Type = None; Errors = [] }
    ListDictionary.set symbols (Map.add (kind, name) entity nestedDict) ctx.Table
    entity    

/// Assign entity to a node in parse tree
let setEntity ctx node entity = 
  ctx.Bound.Add(node.Range, entity)
  node.Entity <- Some entity
  entity

/// Used for entities with no name
let anonymous = { Name.Name = "" }

/// Bind entities to expressions in the parse tree
/// (See `EntityKind` for explanation of how the entity tree looks like)
let rec bindExpression callSite ctx node = 
  let bindCallArgExpression site = bindExpression (Some site)
  let bindExpression = bindExpression None
  match node.Node with
  | Expr.Variable(name) ->
      match ctx.Variables.TryFind name.Node with 
      | Some decl -> bindEntity ctx EntityKind.Variable [decl] name.Node |> setEntity ctx node
      | None -> bindEntity ctx EntityKind.GlobalValue [ctx.Scope] name.Node |> setEntity ctx node

  | Expr.Call(instExpr, name, argsNode) ->
      // Bind instance & create call site that depends on it
      let inst = defaultArg (Option.map (bindExpression ctx) instExpr) ctx.Root
      let site arg = bindEntity ctx (EntityKind.CallSite arg) [inst] name.Node
      // Bind arguments - which depend on the call site
      let args = argsNode.Node |> List.mapi (fun idx arg -> 
        let site = site (match arg.Name with Some n -> Choice1Of2 n.Node.Name | _ -> Choice2Of2 idx)
        let expr = bindCallArgExpression site ctx arg.Value
        match arg.Name with 
        | Some n -> bindEntity ctx EntityKind.NamedParam [expr] n.Node |> setEntity ctx n
        | None -> expr)
      let args = bindEntity ctx EntityKind.ArgumentList (ctx.Root::args) anonymous |> setEntity ctx argsNode
      let named = bindEntity ctx EntityKind.NamedMember [ctx.Root] name.Node |> setEntity ctx name
      bindEntity ctx (EntityKind.ChainElement(instExpr.IsSome, false)) [named; inst; args] name.Node |> setEntity ctx node 

  | Expr.Property(expr, name) ->
      let ante = bindExpression ctx expr
      let named = bindEntity ctx EntityKind.NamedMember [ctx.Root] name.Node |> setEntity ctx name      
      bindEntity ctx (EntityKind.ChainElement(true, true)) [named; ante] name.Node |> setEntity ctx node 

  | Expr.Binary(l, op, r) ->
      let lentity = bindExpression ctx l
      let rentity = bindExpression ctx r
      bindEntity ctx (EntityKind.Operator op.Node) [lentity; rentity] anonymous |> setEntity ctx node

  | Expr.List(els) ->
      let entities = els |> List.map (bindExpression ctx)      
      bindEntity ctx EntityKind.List entities anonymous |> setEntity ctx node

  | Expr.Function(v, e) ->
      let scope = bindEntity ctx EntityKind.Scope [ctx.Scope] anonymous
      let varParent = match callSite with None -> [scope] | Some s -> [s]
      let var = bindEntity ctx EntityKind.Binding varParent v.Node |> setEntity ctx v
      let body = bindExpression { ctx with Variables = Map.add v.Node var ctx.Variables } e
      bindEntity ctx EntityKind.Function [var; body] anonymous |> setEntity ctx node

  | Expr.Boolean b -> bindEntity ctx (EntityKind.Constant (Constant.Boolean b)) [ctx.Root] anonymous |> setEntity ctx node
  | Expr.String s -> bindEntity ctx (EntityKind.Constant (Constant.String s)) [ctx.Root] anonymous |> setEntity ctx node
  | Expr.Number n -> bindEntity ctx (EntityKind.Constant (Constant.Number n)) [ctx.Root] anonymous |> setEntity ctx node
  | Expr.Empty -> bindEntity ctx (EntityKind.Constant Constant.Empty) [ctx.Root] anonymous |> setEntity ctx node

/// Bind entities to a command in a parse tree. The handling of `let` is similar
/// to the handling of lambda abstraction. This adds variables to context - we ignore
/// bound entities, because nothing depends on it (except via variables)
let bindCommand ctx node =
  match node.Node with
  | Command.Let(v, e) ->
      let body = bindExpression None ctx e 
      let scope = bindEntity ctx EntityKind.Scope [ctx.Scope] anonymous
      let var = bindEntity ctx EntityKind.Variable [body] v.Node |> setEntity ctx v
      let node = bindEntity ctx EntityKind.Command [var; body] anonymous |> setEntity ctx node
      { ctx with Variables = Map.add v.Node var ctx.Variables }, node

  | Command.Expr(e) ->
      let body = bindExpression None ctx e 
      let node = bindEntity ctx EntityKind.Command [body] anonymous|> setEntity ctx node
      ctx, node

/// Bind entities to all nodes in the program
let bindProgram ctx (program:Program) =
  ctx.Bound.Clear()
  let _, ents = 
    program.Body.Node |> List.fold (fun (ctx, nodes) cmd -> 
      let ctx, node = bindCommand ctx cmd
      ctx, node::nodes) (ctx, [])  
  bindEntity ctx EntityKind.Program ents anonymous,
  ctx.Bound.ToArray()
  
/// Create a new binding context - this stores cached entities
let createContext name =
  let root = 
    { Kind = EntityKind.Root; Antecedents = []; Errors = []
      Name = { Name = name }; Symbol = createSymbol(); Type = None }
  { Table = System.Collections.Generic.Dictionary<_, _>(); 
    Bound = ResizeArray<_>(); Variables = Map.empty; 
    Scope = root; Root = root }
