#if INTERACTIVE
#r "../../src/thegamma/bin/Debug/thegamma.dll"
#r "../../packages/NUnit/lib/net45/nunit.framework.dll"
#else
[<NUnit.Framework.TestFixture>]
module TheGamma.Tests.Parser
#endif
open TheGamma
open TheGamma.Ast
open NUnit.Framework

// --------------------------------------------------------------------------------------
// Helpers for writing tests for parser
// --------------------------------------------------------------------------------------

/// Type-safe assertion
let equal (expected:'T) (actual:'T) = Assert.AreEqual(expected, actual)

/// Assert that ranges of all names in parsed tree are correct
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
    equal (Ast.escapeIdent n.Node.Name) 
      (code.Substring(n.Range.Start, n.Range.End - n.Range.Start + 1))

/// Assert that result contains given errors
let assertErrors expectErrors ((code:string), cmds, errs) = 
  equal (List.length expectErrors) (List.length errs)
  for (en, ec), (an, (astart, aend)) in List.zip expectErrors errs do
    let s = code.Substring(astart, aend-astart+1)
    equal en an
    equal ec s

/// Assert that expression contains given sub-expression
let rec hasSubExpr f e =
  if f e then true else
    match e with 
    | ExprNode(es, _) -> es |> List.exists (fun e -> hasSubExpr f e.Node)
    | ExprLeaf _ -> false

/// Assert that result contains given sub-expression
let assertSubExpr f (code, (cmds:Node<Command> list), errs) = 
  let matches = 
    cmds |> List.exists (fun cmd ->
      match cmd.Node with
      | Command.Expr e -> hasSubExpr f e.Node
      | Command.Let(_, e) -> hasSubExpr f e.Node )
  equal true matches

/// Assert that result contains binding command with body
let assertLet n f (code, (cmds:Node<Command> list), errs) = 
  let matches = cmds |> List.exists (fun cmd ->
    match cmd.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | _ -> false)
  equal true matches

/// Assert that result consists of the specified commands
let assertCmds ns fs (code, (cmds:Node<Command> list), errs) = 
  equal (List.length ns) (List.length cmds) 
  let matches = List.zip3 ns fs cmds |> List.forall (fun (n, f, c) -> 
    match c.Node with
    | Command.Let(id, e) when id.Node.Name = n -> f e.Node
    | Command.Expr(e) when n = "" -> f e.Node
    | _ -> false )
  equal true matches

/// Sub-expression contains a function with a given name and body
let isFun name bf = function 
  | Expr.Function(n, b) -> n.Node.Name = name && bf b.Node | _ -> false

/// Sub-expression contains property with given name
let isProperty name = function 
  | Expr.Member(_, { Node = Expr.Variable n }) -> n.Node.Name = name | _ -> false

/// Sub-expression contains property a placeholder
let isPlaceholder name checkBody = function 
  | Expr.Placeholder(id, body) -> id.Node.Name = name && checkBody body.Node | _ -> false

/// Sub-expression contains empty expression
let isEmpty = function 
  | Expr.Empty -> true | _ -> false

/// Sub-expression contains call with given name and arguments match function
let isCall name ac = function 
  | Expr.Call({ Node = Expr.Variable n }, args) 
  | Expr.Call({ Node = Expr.Member(_, { Node = Expr.Variable n }) }, args) -> 
      n.Node.Name = name && ac args | _ -> false

/// Sub-expression is a list with elements matching specified functions
let isList conds = function 
  | Expr.List(elems) -> List.zip conds elems |> List.forall (fun (cond, el) -> cond el.Node) | _ -> false

/// Expression is binary operator
let isBinary op fl fr = function
  | Expr.Binary(l, o, r) -> o.Node = op && fl l.Node && fr r.Node
  | _ -> false

/// Expression is specified value (string, int, float, bool)
let isVal (v:obj) e = 
  match e with
  | Expr.Number(n) -> (unbox v) = n
  | Expr.String(s) -> (unbox v) = s
  | Expr.Boolean(b) -> (unbox v) = b
  | _ -> false

/// Matches anything
let any _ = true

/// Specify conditions on arguments
let hasArgValues conds { Node = args } = 
  List.zip conds args |> List.forall (fun (f, (arg:Argument)) -> f arg.Value.Node)

/// Specify conditions on argument names
let hasArgNames names { Node = args } = 
  List.zip names args |> List.forall (fun (n, (arg:Argument)) -> 
    n = defaultArg (arg.Name |> Option.map (fun i -> i.Node.Name)) "" )

/// Sub-expression contains variable with given name
let isVariable name = function 
  | Expr.Variable(n) -> n.Node.Name = name | _ -> false

/// Tokenize & parse test code
let parse (code:string) = 
  let code = code.Replace("\r", "").Replace("\n    ","\n")
  let res, errs = Parser.parseProgram code
  for e in errs do if e.Number = 299 then failwith e.Message
  code, res.Body.Node, [ for e in errs -> e.Number, (e.Range.Start, e.Range.End) ]

/// Format binary operaation for testing purposes
let formatSimpleNumExpr e = 
  let rec loop = function
    | { Node = Expr.Number n } -> string (int n)
    | { Node = Expr.Binary(l, op, r) } -> 
        sprintf "(%s %s %s)" (loop l) (Ast.formatToken (TokenKind.Operator op.Node)) (loop r)
    | _ -> "?"
  loop (node { Start = 0; End = 0 } e)

// --------------------------------------------------------------------------------------
// TESTS: Call chains and nesting
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse indented call chain``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
      yadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertErrors []

[<Test>]
let ``Correctly parse indented call chain with calls``() =
  let actual = parse """
    let a = foo(1).
      'bar zoo'(2).
      yadda"""
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 2.0]))
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertErrors []

[<Test>]
let ``Correctly parse aligned call chain without nesting``() =
  let actual = parse """
    foo.
      bar"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors []

[<Test>]
let ``Error reported on identifier following an unfinished chain``() =
  let actual = parse """
    foo.
    bar"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isVariable "bar")
  actual |> assertErrors [202, "."]

[<Test>]
let ``Correctly parse indented call chain with nesting``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
          yadda.
      dadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertSubExpr (isProperty "dadda")
  actual |> assertErrors []

[<Test>]
let ``Correctly parse named arguments of method call``() =
  let actual = parse """
    foo(a=bar(b=1, c=2, 3))"""
  actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
  actual |> assertSubExpr (isCall "bar" (hasArgNames ["b"; "c"; ""]))
  actual |> assertErrors []

[<Test>]
let ``Error reported on incomplete named parameter specification``() =
  let actual = parse """
    foo(a=bar(b=, c=2, 3))"""
  actual |> assertSubExpr (isCall "foo" (hasArgNames ["a"]))
  actual |> assertSubExpr (isCall "bar" (hasArgNames ["b"; "c"; ""]))
  actual |> assertErrors [207, "="]

[<Test>]
let ``Currectly parse nested chain as equality test``() =
  let actual = parse "foo(bar.yadda=1)"
  actual |> assertSubExpr (isCall "foo" (hasArgNames [""]))
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertErrors []

[<Test>]
let ``Error reported on keyword after a call chain``() =
  let actual = parse """
    let a = foo.let"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertErrors [201, "let"]

[<Test>]
let ``Error reported on unfinished call chain``() =
  let actual = parse """
    let a = foo.
      bar.
    let b = a"""
  actual |> assertSubExpr (isVariable "foo")
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors [202, "."]

[<Test>]
let ``Error reported on wrongly indented chain``() =
  let actual = parse """
    let a = foo.
      'bar zoo'.
    yadda"""
  actual |> assertSubExpr (isProperty "bar zoo")
  actual |> assertSubExpr (isVariable "yadda")
  actual |> assertErrors [202, "."]

[<Test>]
let ``Empty property parsed after incomplete '.' followed by identifier``() =
  // (this models the case when we start typing .something after 'bar zoo')
  let actual = parse """
    let a = foo.
      'bar zoo'.
    yadda"""
  actual |> assertSubExpr (isProperty "")
  actual |> assertErrors [202, "."]

[<Test>]
let ``Empty property parsed after incomplete '.' followed by another '.'``() = 
  // (this models the case when we start typing .something after 'bar zoo')
  let actual = parse """
    let a = foo.
      'bar zoo'.
      .foo"""
  actual |> assertSubExpr (isProperty "")
  actual |> assertErrors [201, "."]

// --------------------------------------------------------------------------------------
// TESTS: Call chains with arguments
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse call with arguments``() =
  let actual = parse """
    let a = foo.
      'bar zoo'(1)"""
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))

[<Test>]
let ``Error reported when argument list is not closed``() =
  let actual = parse """
    let a = foo.
      'bar zoo'("""
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues []))
  actual |> assertErrors [208, "("]

[<Test>]
let ``Correctly parse multiple nested chain calls`` () =
  let actual = parse """
    let a = foo1
      .foo2(bar1
        .bar2(goo1
          .goo2))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parse nested chain with two space indentation`` () =
  let actual = parse """
    let a = foo1
      .foo2(bar1
      .bar2(goo1
      .goo2.'bar zoo'(1)))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertSubExpr (isCall "bar zoo" (hasArgValues [isVal 1.0]))

[<Test>]
let ``Correctly parse one inline chain and one indented chain`` () =
  let actual = parse """
    let a = foo1.foo2(bar1.bar2(
      goo1.goo2))"""
  actual |> assertSubExpr (isCall "bar2" (hasArgValues [hasSubExpr (isProperty "goo2") ]))
  actual |> assertErrors []


[<Test>]
let ``Report error and stop parsing in unindented argument list`` () =
  let actual = parse """
    let a = foo(
        1,
    bar(2)"""
  actual |> assertErrors [208,","]
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
  actual |> assertSubExpr (isCall "bar" (hasArgValues [isVal 2.0]))

[<Test>]
let ``Report error and continue parsing indented method chain`` () =
  let actual = parse """
    let a = foo(
        1,).yadda
    bar(2)"""
  actual |> assertErrors [207, ","]
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal 1.0]))
  actual |> assertSubExpr (isProperty "yadda")

[<Test>]
let ``Correctly parse chain with calls and properties`` () = 
  let actual = parse """
    let a = goo.foo("yo").'some bar'
    yadda(2)"""
  actual |> assertSubExpr (isCall "foo" (hasArgValues [isVal "yo"]))
  actual |> assertSubExpr (isProperty "some bar")
  actual |> assertErrors []

// --------------------------------------------------------------------------------------
// TESTS: List expressions
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse list of inline elements`` () =
  let actual = parse """
    let a = [1, 2, 3]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
  actual |> assertErrors []

[<Test>]
let ``Report error on unexpected comma in the middle of list expression`` () =
  let actual = parse """
    let a = [1, , 3]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 3.0])
  actual |> assertErrors [212, ","]

[<Test>]
let ``Report error on unexpected comma at the end of list expression`` () =
  let actual = parse """
    let a = [1, 2, ]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0])
  actual |> assertErrors [212, ","]

[<Test>]
let ``Correctly parse list of elements with line breaks`` () =
  let actual = parse """
    let a = [1, 
      2, 
      3]"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0; isVal 3.0])
  actual |> assertErrors []

[<Test>]
let ``Report error and stop parsing unindented list`` () =
  let actual = parse """
    let a = [1, 
    foo.bar"""
  actual |> assertSubExpr (isProperty "bar")
  actual |> assertErrors [213, ","]

[<Test>]
let ``Report error and skip over unexpected tokens in list`` () =
  let actual = parse """
    let a = [1,
      2,
      +] 
    foo.bar"""
  actual |> assertSubExpr (isList [isVal 1.0; isVal 2.0])
  actual |> assertErrors [212, "+"]

// --------------------------------------------------------------------------------------
// TESTS: Commands
// --------------------------------------------------------------------------------------

[<Test>]
let ``Report error on expression after completed expression command`` () = 
  let actual = parse """
    1 2
    let b = 3"""
  actual |> assertErrors [216, "2"]
  actual |> assertLet "b" (hasSubExpr (isVal 3.0))
  actual |> assertSubExpr (isVal 1.0)

[<Test>]
let ``Report error on invalid syntax aft the beginning of a command`` () = 
  let actual = parse """
    1 
    : 2
    let b = 3"""
  actual |> assertErrors [216, ":"]
  actual |> assertLet "b" (hasSubExpr (isVal 3.0))
  actual |> assertSubExpr (isVal 1.0)

[<Test>]
let ``Report error on unifinished let and continue parsing`` () =
  let actual = parse """
    let 
    let b = 1"""
  actual |> assertErrors [215, "let"]
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))

[<Test>]
let ``Report error and treat missing identifier as empty string`` () =
  let actual = parse """
    let 1 + 2
    let b = 1"""
  actual |> assertLet "" (fun e -> equal "(1 + 2)" (formatSimpleNumExpr e); true)
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))
  actual |> assertErrors [214, "1"]

[<Test>]
let ``Report error on let without name or body`` () =
  let actual = parse """
    let +
    let b = 1"""
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))
  actual |> assertErrors [214, "+"]

[<Test>]
let ``Report error on let with name and no equals, but parse it anyway`` () =
  let actual = parse """
    let a foo.bar
    let b = 1"""
  actual |> assertErrors [214, "foo"]
  actual |> assertLet "a" (hasSubExpr (isProperty "bar"))
  actual |> assertLet "b" (hasSubExpr (isVal 1.0))

[<Test>]
let ``Report error on nested expression and parse it as command`` () =
  let actual = parse """
    1 
      3
        4 + 1
    let b = 1"""
  actual |> assertCmds [""; ""; "b"] [any; any; any]
  actual |> assertErrors [216, "3"]
  actual |> assertSubExpr (isVal 1.0)
  
[<Test>]
let ``Report error on nested expression after let and parse it as command`` () =
  let actual = parse """
    let a = 1 
      2
    let b = 3"""
  actual |> assertCmds ["a"; ""; "b"] [hasSubExpr (isVal 1.0); any; hasSubExpr (isVal 3.0)]
  actual |> assertErrors [216, "2"]

[<Test>]
let ``Correctly parse mix of let bindings and expression commands`` () =
  let actual = parse """
    let a = 1 + 2 
    3
    let b = 1"""
  actual |> assertCmds ["a";"";"b"] [ any; hasSubExpr (isVal 3.0); any ]
  actual |> assertErrors []

[<Test>]
let ``Report error on expression after let binding and parse it as command`` () =
  let actual = parse """
    let a = 1 2
    let b = 3"""
  actual |> assertCmds ["a"; ""; "b"] [hasSubExpr (isVal 1.0); any; hasSubExpr (isVal 3.0)]
  actual |> assertErrors [216, "2"]

// --------------------------------------------------------------------------------------
// TESTS: Binary operator precedence & parenthesis
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parses precedence of numerical operators`` () =
  let actual = parse "foo(1 + 2 ^ 3 * 4)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "(1 + ((2 ^ 3) * 4))" (formatSimpleNumExpr e)
    true ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parses precedence of Boolean and numerical operators`` () =
  let actual = parse "foo(2 ^ 3 * 4 > 1 + 2 = 1 > 8)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "((((2 ^ 3) * 4) > (1 + 2)) = (1 > 8))" (formatSimpleNumExpr e) 
    true ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parses precedence of operators with nested chain`` () =
  let actual = parse """
    let a = 
      foo(1 + 2 ^ bar.
      yadda * 4)"""
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "(1 + ((2 ^ ?) * 4))" (formatSimpleNumExpr e) 
    true ]))
  actual |> assertErrors []

[<Test>]
let ``Correctly parses parenthesized expression`` () =
  let actual = parse "foo(1 + (2 + 3) + 4)"
  actual |> assertSubExpr (isCall "foo" (hasArgValues [ fun e -> 
    equal "((1 + (2 + 3)) + 4)" (formatSimpleNumExpr e) 
    true ]))

[<Test>]
let ``Report erroneous token inside parenthesized expression`` () =
  let actual = parse "foo(1 + (2 let) + 4)"
  actual |> assertErrors [209, "let"]

[<Test>]
let ``Report erroneous end of nesting and continue parsing`` () =
  let actual = parse """
    foo(1 + (2
    bar.yadda"""
  actual |> assertErrors [210, "2"; 208, "("]
  actual |> assertSubExpr (isProperty "yadda")
  actual |> assertSubExpr (isVariable "bar")

// --------------------------------------------------------------------------------------
// TESTS: Explicit functions
// --------------------------------------------------------------------------------------

[<Test>]
let ``Correctly parse function with multi-line body``() =
  let actual = parse """
    foo(fun x -> 
      some
        .nested
        .chain)"""
  actual |> assertErrors []
  actual |> assertSubExpr (isFun "x" (hasSubExpr (isProperty "chain")))

[<Test>]
let ``Report error when function is missing variable and body``() =
  let actual = parse """
    foo(fun)"""
  actual |> assertErrors [217, ")"]
  actual |> assertSubExpr (isFun "" isEmpty)

[<Test>]
let ``Report error when function is missing arrow and body``() =
  let actual = parse """
    foo(fun x)"""
  actual |> assertErrors [218, ")"]
  actual |> assertSubExpr (isFun "x" isEmpty)

[<Test>]
let ``Report error when function is missing arrow``() =
  let actual = parse """
    foo(fun x 1 + 2)"""
  actual |> assertErrors [218, "1"]
  actual |> assertSubExpr (isFun "x" (hasSubExpr (isVal 2.0)))

// --------------------------------------------------------------------------------------
// TESTS: Placeholders
// --------------------------------------------------------------------------------------

[<Test>]
let ``Can parse placeholder in member chain``() =
  let actual = parse """
    foo.[bar:'some name'].goo"""
  actual |> assertErrors []
  actual |> assertSubExpr (isProperty "goo")
  actual |> assertSubExpr (isPlaceholder "bar" (isVariable "some name"))

[<Test>]
let ``Can parse placeholder containing a chain``() =
  let actual = parse """
    foo.[bar:'some name'.'another name'].goo"""
  actual |> assertErrors []
  actual |> assertSubExpr (isProperty "goo")
  actual |> assertSubExpr (isPlaceholder "bar" (hasSubExpr (isVariable "some name")))
  actual |> assertSubExpr (isPlaceholder "bar" (hasSubExpr (isProperty "another name")))

[<Test>]
let ``Can parse placeholder in multi-line member chain``() =
  let actual = parse """
    foo
      .[bar:
        'some name']
      .goo"""
  actual |> assertErrors []
  actual |> assertSubExpr (isProperty "goo")
  actual |> assertSubExpr (isPlaceholder "bar" (isVariable "some name"))

[<Test>]
let ``Report error when placeholder is missing body``() =
  let actual = parse """
    foo.[bar:].goo"""
  actual |> assertErrors [205, "]"]
  actual |> assertSubExpr (isProperty "goo")
  actual |> assertSubExpr (isPlaceholder "bar" any)

[<Test>]
let ``Report error when placeholder is missing body and ident``() =
  let actual = parse """
    foo.[].goo"""
  actual |> assertErrors [205, "]"]
  actual |> assertSubExpr (isProperty "goo")
  actual |> assertSubExpr (isPlaceholder "" any)

// --------------------------------------------------------------------------------------
// TESTS: Ranges of identifiers
// --------------------------------------------------------------------------------------

[<Test>]
let ``Ranges of identifiers in Olympic sample are correct`` = 
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
