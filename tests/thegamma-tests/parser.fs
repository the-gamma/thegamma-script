module TheGamma.Tests.Parser

open Fable.Mocha
open TheGamma
open TheGamma.Ast

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

let equal (expected:'T) (actual:'T) =
  if expected <> actual then failwithf "Expected '%A' but was '%A'" expected actual

let assertNamesMatch (code:string, cmds, _) =
  let names = ResizeArray<_>()
  let rec collectIdents (e:Node<Expr>) =
    match e.Node with
    | ExprNode(es, ns) ->
        names.AddRange(ns)
        for e in es do collectIdents e
    | _ -> ()
  for c in cmds do
    match c.Node with
    | Command.Expr(e) -> collectIdents e
    | Command.Let(n, e) -> names.Add(n); collectIdents e
  for n in names do
    let source = code.Substring(n.Range.Start, n.Range.End - n.Range.Start + 1)
    let source = if source = "" then "''" else source
    equal (Ast.escapeIdent n.Node.Name) source

let assertErrors expectErrors ((code:string), cmds, errs) =
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, (astart, aend)) in List.zip expectErrors errs do
    let s = code.Substring(astart, aend-astart+1)
    equal en an
    equal ec s

let rec hasSubExpr f e =
  if f e then true else
    match e with
    | ExprNode(es, _) -> es |> List.exists (fun e -> hasSubExpr f e.Node)
    | ExprLeaf _ -> false

let assertSubExpr f (code, (cmds:Node<Command> list), errs) =
  let matches =
    cmds |> List.exists (fun cmd ->
      match cmd.Node with
      | Command.Expr e -> hasSubExpr f e.Node
      | Command.Let(_, e) -> hasSubExpr f e.Node )
  equal true matches

let assertLet n f (code, (cmds:Node<Command> list), errs) =
  let matches = cmds |> List.exists (fun cmd ->
    match cmd.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | _ -> false)
  equal true matches

let assertCmds ns fs (code, (cmds:Node<Command> list), errs) =
  equal (List.length ns) (List.length cmds)
  let matches = List.zip3 ns fs cmds |> List.forall (fun (n, f, c) ->
    match c.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | Command.Expr(e) when n = "" -> f e.Node
    | _ -> false )
  equal true matches

let isFun name bf = function
  | Expr.Function(n, b) -> n.Node.Name = name && bf b.Node | _ -> false

let isProperty name = function
  | Expr.Member(_, { Node = Expr.Variable n }) -> n.Node.Name = name | _ -> false

let isPlaceholder name checkBody = function
  | Expr.Placeholder(id, body) -> id.Node.Name = name && checkBody body.Node | _ -> false

let isEmpty = function
  | Expr.Empty -> true | _ -> false

let isCall name ac = function
  | Expr.Call({ Node = Expr.Variable n }, args)
  | Expr.Call({ Node = Expr.Member(_, { Node = Expr.Variable n }) }, args) ->
      n.Node.Name = name && ac args | _ -> false

let isList conds = function
  | Expr.List(elems) -> List.zip conds elems |> List.forall (fun (cond, el) -> cond el.Node) | _ -> false

let isBinary op fl fr = function
  | Expr.Binary(l, o, r) -> o.Node = op && fl l.Node && fr r.Node
  | _ -> false

let isVal (v:obj) e =
  match e with
  | Expr.Number(n) -> (unbox v) = n
  | Expr.String(s) -> (unbox v) = s
  | Expr.Boolean(b) -> (unbox v) = b
  | _ -> false

let any _ = true

let hasArgValues conds { Node = args } =
  List.zip conds args |> List.forall (fun (f, (arg:Argument)) -> f arg.Value.Node)

let hasArgNames names { Node = args } =
  List.zip names args |> List.forall (fun (n, (arg:Argument)) ->
    n = defaultArg (arg.Name |> Option.map (fun i -> i.Node.Name)) "" )

let isVariable name = function
  | Expr.Variable(n) -> n.Node.Name = name | _ -> false

let parse (code:string) =
  let code = code.Replace("\r", "").Replace("\n      ","\n")
  let res, errs = Parser.parseProgram code
  for e in errs do if e.Number = 299 then failwith e.Message
  code, res.Body.Node, [ for e in errs -> e.Number, (e.Range.Start, e.Range.End) ]

let formatSimpleNumExpr e =
  let rec loop = function
    | { Node = Expr.Number n } -> string (int n)
    | { Node = Expr.Binary(l, op, r) } ->
        sprintf "(%s %s %s)" (loop l) (Ast.formatToken (TokenKind.Operator op.Node)) (loop r)
    | _ -> "?"
  loop (node { Start = 0; End = 0 } e)

// --------------------------------------------------------------------------------------
// TESTS
// --------------------------------------------------------------------------------------

let tests = testList "Parser" [
  testCase "Correctly parse indented call chain" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'.
        yadda"""
    actual |> assertSubExpr (isProperty "bar zoo")
    actual |> assertSubExpr (isProperty "yadda")
    actual |> assertErrors []

  testCase "Correctly parse indented call chain with calls" <| fun () ->
    let actual = parse """
      let a = foo(1).
        'bar zoo'(2).
        yadda"""
    actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
    actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 2.0]))
    actual |> assertSubExpr (isProperty "yadda")
    actual |> assertErrors []

  testCase "Correctly parse aligned call chain without nesting" <| fun () ->
    let actual = parse """
      foo.
        bar"""
    actual |> assertSubExpr (isVariable "foo")
    actual |> assertSubExpr (isProperty "bar")
    actual |> assertErrors []

  testCase "Error reported on identifier following an unfinished chain" <| fun () ->
    let actual = parse """
      foo.
      bar"""
    actual |> assertSubExpr (isVariable "foo")
    actual |> assertSubExpr (isVariable "bar")
    actual |> assertErrors [202, "."]

  testCase "Correctly parse indented call chain with nesting" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'.
            yadda.
        dadda"""
    actual |> assertSubExpr (isProperty "bar zoo")
    actual |> assertSubExpr (isProperty "yadda")
    actual |> assertSubExpr (isProperty "dadda")
    actual |> assertErrors []

  testCase "Correctly parse named arguments of method call" <| fun () ->
    let actual = parse """
      foo(a=bar(b=1, c=2, 3))"""
    actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
    actual |> assertSubExpr (isCall "bar" (hasArgNames ["b"; "c"; ""]))
    actual |> assertErrors []

  testCase "Error reported on incomplete named parameter specification" <| fun () ->
    let actual = parse """
      foo(a=bar(b=, c=2, 3))"""
    actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
    actual |> assertSubExpr (isCall "bar" (hasArgNames ["b"; "c"; ""]))
    actual |> assertErrors [207, "="]

  testCase "Correctly parse nested chain as equality test" <| fun () ->
    let actual = parse "foo(bar.yadda=1)"
    actual |> assertSubExpr (isCall "foo" (hasArgNames [""]))
    actual |> assertSubExpr (isProperty "yadda")
    actual |> assertErrors []

  testCase "Error reported on keyword after a call chain" <| fun () ->
    let actual = parse """
      let a = foo.let"""
    actual |> assertSubExpr (isVariable "foo")
    actual |> assertErrors [201, "let"]

  testCase "Error reported on unfinished call chain" <| fun () ->
    let actual = parse """
      let a = foo.
        bar.
      let b = a"""
    actual |> assertSubExpr (isVariable "foo")
    actual |> assertSubExpr (isProperty "bar")
    actual |> assertErrors [202, "."]

  testCase "Error reported on wrongly indented chain" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'.
      yadda"""
    actual |> assertSubExpr (isProperty "bar zoo")
    actual |> assertSubExpr (isVariable "yadda")
    actual |> assertErrors [202, "."]

  testCase "Empty property parsed after incomplete '.' followed by identifier" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'.
      yadda"""
    actual |> assertSubExpr (isProperty "")
    actual |> assertErrors [202, "."]

  testCase "Empty property parsed after incomplete '.' followed by another '.'" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'.
        .foo"""
    actual |> assertSubExpr (isProperty "")
    actual |> assertErrors [201, "."]

  testCase "Correctly parse call with arguments" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'(1)"""
    actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))

  testCase "Error reported when argument list is not closed" <| fun () ->
    let actual = parse """
      let a = foo.
        'bar zoo'("""
    actual |> assertSubExpr (isCall "bar zoo" (hasArgValues []))
    actual |> assertErrors [208, "("]

  testCase "Correctly parse multiple nested chain calls" <| fun () ->
    let actual = parse """
      let a = foo1
        .foo2(bar1
          .bar2(goo1
            .goo2))"""
    actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
    actual |> assertErrors []

  testCase "Correctly parse nested chain with two space indentation" <| fun () ->
    let actual = parse """
      let a = foo1
        .foo2(bar1
        .bar2(goo1
        .goo2.'bar zoo'(1)))"""
    actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
    actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))

  testCase "Correctly parse one inline chain and one indented chain" <| fun () ->
    let actual = parse """
      let a = foo1.foo2(bar1.bar2(
        goo1.goo2))"""
    actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
    actual |> assertErrors []

  testCase "Report error and stop parsing in unindented argument list" <| fun () ->
    let actual = parse """
      let a = foo(
          1,
      bar(2)"""
    actual |> assertErrors [208,","]
    actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
    actual |> assertSubExpr (isCall "bar" (hasArgValues [isVal 2.0]))

  testCase "Report error and continue parsing indented method chain" <| fun () ->
    let actual = parse """
      let a = foo(
          1,).yadda
      bar(2)"""
    actual |> assertErrors [207, ","]
    actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
    actual |> assertSubExpr (isProperty "yadda")

  testCase "Correctly parse chain with calls and properties" <| fun () ->
    let actual = parse """
      let a = goo.foo("yo").'some bar'
      yadda(2)"""
    actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal "yo"]))
    actual |> assertSubExpr (isProperty "some bar")
    actual |> assertErrors []

  testCase "Correctly parse list of inline elements" <| fun () ->
    let actual = parse """
      let a = [1, 2, 3]"""
    actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
    actual |> assertErrors []

  testCase "Report error on unexpected comma in the middle of list expression" <| fun () ->
    let actual = parse """
      let a = [1, , 3]"""
    actual |> assertSubExpr (isList [isVal 1.0; isVal 3.0])
    actual |> assertErrors [212, ","]

  testCase "Report error on unexpected comma at the end of list expression" <| fun () ->
    let actual = parse """
      let a = [1, 2, ]"""
    actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0])
    actual |> assertErrors [212, ","]

  testCase "Correctly parse list of elements with line breaks" <| fun () ->
    let actual = parse """
      let a = [1,
        2,
        3]"""
    actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
    actual |> assertErrors []

  testCase "Report error and stop parsing unindented list" <| fun () ->
    let actual = parse """
      let a = [1,
      foo.bar"""
    actual |> assertSubExpr (isProperty "bar")
    actual |> assertErrors [213, ","]

  testCase "Report error and skip over unexpected tokens in list" <| fun () ->
    let actual = parse """
      let a = [1,
        2,
        +]
      foo.bar"""
    actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0])
    actual |> assertErrors [212, "+"]

  testCase "Report error on expression after completed expression command" <| fun () ->
    let actual = parse """
      1 2
      let b = 3"""
    actual |> assertErrors [216, "2"]
    actual |> assertLet "b" (hasSubExpr (isVal 3.0))
    actual |> assertSubExpr (isVal 1.0)

  testCase "Report error on invalid syntax at the beginning of a command" <| fun () ->
    let actual = parse """
      1
      : 2
      let b = 3"""
    actual |> assertErrors [216, ":"]
    actual |> assertLet "b" (hasSubExpr (isVal 3.0))
    actual |> assertSubExpr (isVal 1.0)

  testCase "Report error on unfinished let and continue parsing" <| fun () ->
    let actual = parse """
      let
      let b = 1"""
    actual |> assertErrors [215, "let"]
    actual |> assertLet "b" (hasSubExpr (isVal 1.0))

  testCase "Report error and treat missing identifier as empty string" <| fun () ->
    let actual = parse """
      let 1 + 2
      let b = 1"""
    actual |> assertLet "" (fun e -> equal "(1 + 2)" (formatSimpleNumExpr e); true)
    actual |> assertLet "b" (hasSubExpr (isVal 1.0))
    actual |> assertErrors [214, "1"]

  testCase "Report error on let without name or body" <| fun () ->
    let actual = parse """
      let +
      let b = 1"""
    actual |> assertLet "b" (hasSubExpr (isVal 1.0))
    actual |> assertErrors [214, "+"]

  testCase "Report error on let with name and no equals, but parse it anyway" <| fun () ->
    let actual = parse """
      let a foo.bar
      let b = 1"""
    actual |> assertErrors [214, "foo"]
    actual |> assertLet "a" (hasSubExpr (isProperty "bar"))
    actual |> assertLet "b" (hasSubExpr (isVal 1.0))

  testCase "Report error on nested expression and parse it as command" <| fun () ->
    let actual = parse """
      1
        3
          4 + 1
      let b = 1"""
    actual |> assertCmds [""; ""; "b"] [any; any; any]
    actual |> assertErrors [216, "3"]
    actual |> assertSubExpr (isVal 1.0)

  testCase "Report error on nested expression after let and parse it as command" <| fun () ->
    let actual = parse """
      let a = 1
        2
      let b = 3"""
    actual |> assertCmds ["a"; ""; "b"] [hasSubExpr (isVal 1.0); any; hasSubExpr (isVal 3.0)]
    actual |> assertErrors [216, "2"]

  testCase "Correctly parse mix of let bindings and expression commands" <| fun () ->
    let actual = parse """
      let a = 1 + 2
      3
      let b = 1"""
    actual |> assertCmds ["a";"";"b"] [ any; hasSubExpr (isVal 3.0); any ]
    actual |> assertErrors []

  testCase "Report error on expression after let binding and parse it as command" <| fun () ->
    let actual = parse """
      let a = 1 2
      let b = 3"""
    actual |> assertCmds ["a"; ""; "b"] [hasSubExpr (isVal 1.0); any; hasSubExpr (isVal 3.0)]
    actual |> assertErrors [216, "2"]

  testCase "Correctly parses precedence of numerical operators" <| fun () ->
    let actual = parse "foo(1 + 2 ^ 3 * 4)"
    actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e ->
      equal "(1 + ((2 ^ 3) * 4))" (formatSimpleNumExpr e)
      true ]))
    actual |> assertErrors []

  testCase "Correctly parses precedence of Boolean and numerical operators" <| fun () ->
    let actual = parse "foo(2 ^ 3 * 4 > 1 + 2 = 1 > 8)"
    actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e ->
      equal "((((2 ^ 3) * 4) > (1 + 2)) = (1 > 8))" (formatSimpleNumExpr e)
      true ]))
    actual |> assertErrors []

  testCase "Correctly parses precedence of operators with nested chain" <| fun () ->
    let actual = parse """
      let a =
        foo(1 + 2 ^ bar.
        yadda * 4)"""
    actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e ->
      equal "(1 + ((2 ^ ?) * 4))" (formatSimpleNumExpr e)
      true ]))
    actual |> assertErrors []

  testCase "Correctly parses parenthesized expression" <| fun () ->
    let actual = parse "foo(1 + (2 + 3) + 4)"
    actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e ->
      equal "((1 + (2 + 3)) + 4)" (formatSimpleNumExpr e)
      true ]))

  testCase "Report erroneous token inside parenthesized expression" <| fun () ->
    let actual = parse "foo(1 + (2 let) + 4)"
    actual |> assertErrors [209, "let"]

  testCase "Report erroneous end of nesting and continue parsing" <| fun () ->
    let actual = parse """
      foo(1 + (2
      bar.yadda"""
    actual |> assertErrors [210, "2"; 208, "("]
    actual |> assertSubExpr (isProperty "yadda")
    actual |> assertSubExpr (isVariable "bar")

  testCase "Correctly parse function with multi-line body" <| fun () ->
    let actual = parse """
      foo(fun x ->
        some
          .nested
          .chain)"""
    actual |> assertErrors []
    actual |> assertSubExpr (isFun "x" (hasSubExpr (isProperty "chain")))

  testCase "Report error when function is missing variable and body" <| fun () ->
    let actual = parse """
      foo(fun)"""
    actual |> assertErrors [217, ")"]
    actual |> assertSubExpr (isFun "" isEmpty)

  testCase "Report error when function is missing arrow and body" <| fun () ->
    let actual = parse """
      foo(fun x)"""
    actual |> assertErrors [218, ")"]
    actual |> assertSubExpr (isFun "x" isEmpty)

  testCase "Report error when function is missing arrow" <| fun () ->
    let actual = parse """
      foo(fun x 1 + 2)"""
    actual |> assertErrors [218, "1"]
    actual |> assertSubExpr (isFun "x" (hasSubExpr (isVal 2.0)))

  testCase "Can parse placeholder in member chain" <| fun () ->
    let actual = parse """
      foo.[bar:'some name'].goo"""
    actual |> assertErrors []
    actual |> assertSubExpr (isProperty "goo")
    actual |> assertSubExpr (isPlaceholder "bar" (isVariable "some name"))

  testCase "Can parse placeholder containing a chain" <| fun () ->
    let actual = parse """
      foo.[bar:'some name'.'another name'].goo"""
    actual |> assertErrors []
    actual |> assertSubExpr (isProperty "goo")
    actual |> assertSubExpr (isPlaceholder "bar" (hasSubExpr (isVariable "some name")))
    actual |> assertSubExpr (isPlaceholder "bar" (hasSubExpr (isProperty "another name")))

  testCase "Can parse placeholder in multi-line member chain" <| fun () ->
    let actual = parse """
      foo
        .[bar:
          'some name']
        .goo"""
    actual |> assertErrors []
    actual |> assertSubExpr (isProperty "goo")
    actual |> assertSubExpr (isPlaceholder "bar" (isVariable "some name"))

  testCase "Report error when placeholder is missing body" <| fun () ->
    let actual = parse """
      foo.[bar:].goo"""
    actual |> assertErrors [205, "]"]
    actual |> assertSubExpr (isProperty "goo")
    actual |> assertSubExpr (isPlaceholder "bar" any)

  testCase "Report error when placeholder is missing body and ident" <| fun () ->
    let actual = parse """
      foo.[].goo"""
    actual |> assertErrors [205, "]"]
    actual |> assertSubExpr (isProperty "goo")
    actual |> assertSubExpr (isPlaceholder "" any)

  testCase "Ranges of identifiers in Olympic sample are correct" <| fun () ->
    let actual = parse """
      let phelp =
        olympics.'by athlete'.[athlete:'United States'.'Michael Phelps'].data
          .'group data'.'by Athlete'.'sum Gold'.then
          .'get series'.'with key Athlete'.'and value Gold'

      let data =
        olympics.data
          .'group data'.'by Team'.'sum Gold'.then
          .'sort data'.'by Gold descending'.'and by Team'.then
          .paging.skip(43).take(10)
          .'get series'.'with key Team'.'and value Gold'

      chart.columns([data, phelp], ["#F4C300","#3CB3EC"])
        .legend(position="none")
      """
    actual |> assertErrors []
    actual |> assertNamesMatch

  testCase "Ranges of empty identifier is correct" <| fun () ->
    let actual = parse """
      olympics.foo.
      """
    actual |> assertNamesMatch
]
