namespace TheGamma.Babel

open Fable
open Fable.Core

/// Each Position object consists of a line number (1-indexed) and a column number (0-indexed):
type Position =
  { line: int; column: int; }

type SourceLocation =
  { start: Position; ``end``: Position; }

type AssignmentOperator =
  | AssignEqual
  | AssignMinus
  | AssignPlus
  | AssignMultiply
  | AssignDivide
  | AssignModulus
  | AssignShiftLeft
  | AssignShiftRightSignPropagating
  | AssignShiftRightZeroFill
  | AssignOrBitwise
  | AssignXorBitwise
  | AssignAndBitwise    

type BinaryOperator =
  | BinaryEqual
  | BinaryUnequal
  | BinaryEqualStrict
  | BinaryUnequalStrict
  | BinaryLess
  | BinaryLessOrEqual
  | BinaryGreater
  | BinaryGreaterOrEqual
  | BinaryShiftLeft
  | BinaryShiftRightSignPropagating
  | BinaryShiftRightZeroFill
  | BinaryMinus
  | BinaryPlus
  | BinaryMultiply
  | BinaryDivide
  | BinaryModulus
  | BinaryExponent
  | BinaryOrBitwise
  | BinaryXorBitwise
  | BinaryAndBitwise
  | BinaryIn
  | BinaryInstanceOf

type Pattern = 
  | IdentifierPattern of name:string * location:SourceLocation option

type VariableDeclarationKind = 
  | Var | Let | Const

type VariableDeclarator = 
  | VariableDeclarator of id:Pattern * init:Expression option * location:SourceLocation option

and Expression = 
  | IdentifierExpression of name:string * location:SourceLocation option
  | FunctionExpression of id:string option * ``params``:Pattern list * body:Statement * generator:bool * async:bool * location:SourceLocation option
  | NullLiteral of location:SourceLocation option
  | StringLiteral of value:string * location:SourceLocation option
  | BooleanLiteral of value:bool * location:SourceLocation option
  | NumericLiteral of value:float * location:SourceLocation option
  | AssignmentExpression of operator:AssignmentOperator * left:Expression * right:Expression * location:SourceLocation option
  | CallExpression of callee:Expression * args:Expression list * location:SourceLocation option
  | ArrayExpression of elements:Expression list * location:SourceLocation option
  | MemberExpression of obj:Expression * property:Expression * computed:bool * location:SourceLocation option
  | NewExpression of callee:Expression * arguments:Expression list * location:SourceLocation option
  | BinaryExpression of operator:BinaryOperator * left:Expression * right:Expression * location:SourceLocation option

and Statement =
  | ExpressionStatement of expression:Expression * location:SourceLocation option
  | BlockStatement of body:Statement list * location:SourceLocation option
  | EmptyStatement of location:SourceLocation option
  | ReturnStatement of argument:Expression * location:SourceLocation option
  | VariableDeclaration of kind:VariableDeclarationKind * declarations:VariableDeclarator list * location:SourceLocation option

type Program = 
  { location : SourceLocation option
    body: Statement list }

module Serializer = 
  let createObj props = createObj (List.concat props)
  let inline (=>) k v = [ k, box v ]
  let inline (=?>) k v = match v with Some v -> [ k, box v] | _ -> []

  let serializeBinaryOperator = function
    | BinaryEqual -> "=="
    | BinaryUnequal -> "!="
    | BinaryEqualStrict -> "==="
    | BinaryUnequalStrict -> "!=="
    | BinaryLess -> "<"
    | BinaryLessOrEqual -> "<="
    | BinaryGreater -> ">"
    | BinaryGreaterOrEqual -> ">="
    | BinaryShiftLeft -> "<<"
    | BinaryShiftRightSignPropagating -> ">>"
    | BinaryShiftRightZeroFill -> ">>>"
    | BinaryMinus -> "-"
    | BinaryPlus -> "+"
    | BinaryMultiply -> "*"
    | BinaryDivide -> "/"
    | BinaryModulus -> "%"
    | BinaryExponent -> "**"
    | BinaryOrBitwise -> "|"
    | BinaryXorBitwise -> "^"
    | BinaryAndBitwise -> "&"
    | BinaryIn -> "in"
    | BinaryInstanceOf -> "instanceof"

  let serializeAssignOperator = function
    | AssignEqual -> "="
    | AssignMinus -> "-="
    | AssignPlus -> "+="
    | AssignMultiply -> "*="
    | AssignDivide -> "/="
    | AssignModulus -> "%="
    | AssignShiftLeft -> "<<="
    | AssignShiftRightSignPropagating -> ">>="
    | AssignShiftRightZeroFill -> ">>>="
    | AssignOrBitwise -> "|="
    | AssignXorBitwise -> "^="
    | AssignAndBitwise -> "&="

  let serializePattern pat = 
    match pat with
    | IdentifierPattern(name, loc) ->
        createObj [ "type" => "Identifier"; "name" => name; "loc" =?> loc ]

  let rec serializeDeclarator (VariableDeclarator(id, init, loc)) = 
    createObj [ 
      "type" => "VariableDeclarator"; "id" => serializePattern id; 
      "init" =?> Option.map serializeExpression init; "loc" =?> loc ]
    
  and serializeExpression expr = 
    match expr with
    | IdentifierExpression(name, loc) ->
        createObj [ "type" => "Identifier"; "name" => name; "loc" =?> loc ]
    | NewExpression(callee, args, loc) ->
        createObj [ 
          "type" => "NewExpression"; "callee" => serializeExpression callee
          "arguments" => Array.ofList (List.map serializeExpression args); "loc" =?> loc ]
    | FunctionExpression(id, pars, body, generator, async, loc) ->
        createObj [ 
          "type" => "FunctionExpression"; "id" =?> id; "params" => Array.ofList (List.map serializePattern pars)
          "body" => serializeStatement body; "generator" => generator; "async" => async; "loc" =?> loc ]
    | AssignmentExpression(op, l, r, loc) ->
        createObj [ 
          "type" => "AssignmentExpression"; "left" => serializeExpression l; "right" => serializeExpression r;
          "operator" => serializeAssignOperator op; "loc" =?> loc ]
    | CallExpression(callee, args, loc) ->
        createObj [ 
          "type" => "CallExpression"; "callee" => serializeExpression callee
          "arguments" => Array.ofSeq (List.map serializeExpression args); "loc" =?> loc ]
    | MemberExpression(obj, prop, computed, loc) ->
        createObj [
          "type" => "MemberExpression"; "object" => serializeExpression obj;
          "property" => serializeExpression prop; "computed" => computed; "loc" =?> loc ]
    | BinaryExpression(op, l, r, loc) ->
        createObj [ 
          "type" => "BinaryExpression"; "left" => serializeExpression l; "right" => serializeExpression r;
          "operator" => serializeBinaryOperator op; "loc" =?> loc ]
    
    | ArrayExpression(elements, loc) ->
        createObj [ "type" => "ArrayExpression"; "elements" => Array.ofSeq (List.map serializeExpression elements); "loc" =?> loc ]
    | NullLiteral(loc) ->
        createObj [ "type" => "NullLiteral"; "loc" =?> loc ]
    | StringLiteral(v, loc) ->
        createObj [ "type" => "StringLiteral"; "value" => v; "loc" =?> loc ]
    | BooleanLiteral(v, loc) ->
        createObj [ "type" => "BooleanLiteral"; "value" => v; "loc" =?> loc ]
    | NumericLiteral(v, loc) ->
        createObj [ "type" => "NumericLiteral"; "value" => v; "loc" =?> loc ]

  and serializeStatement stm = 
    match stm with
    | ExpressionStatement(e, loc) ->  
        createObj [ "type" => "ExpressionStatement"; "loc" =?> loc; "expression" => serializeExpression e ]
    | BlockStatement(b, loc) ->
        createObj [ "type" => "BlockStatement"; "loc" =?> loc; "body" => Array.ofList (List.map serializeStatement b) ]
    | EmptyStatement(loc) ->
        createObj [ "type" => "EmptyStatement"; "loc" =?> loc  ]
    | ReturnStatement(arg, loc) ->
        createObj [ "type" => "ReturnStatement"; "loc" =?> loc; "argument" => serializeExpression arg ]
    | VariableDeclaration(kind, decls, loc) ->
        let kind = match kind with Var -> "var" | Let -> "let" | Const -> "const"
        createObj [ 
          "type" => "VariableDeclaration"; "kind" => kind; 
          "declarations" => Array.ofList (List.map serializeDeclarator decls); "loc" =?> loc  ]

  let serializeProgram prog = 
    createObj [
      "type" => "Program"
      "loc" =?> prog.location 
      "sourceType" => "module"
      "body" => Array.ofList (List.map serializeStatement prog.body)
      "directives" => box [||]
    ]

(*
type NumberKind =
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Float32 | Float64

type RegexFlag =
    | RegexGlobal | RegexIgnoreCase | RegexMultiline | RegexSticky

type UnaryOperator =
    | UnaryMinus
    | UnaryPlus
    | UnaryNot
    | UnaryNotBitwise
    | UnaryTypeof
    | UnaryVoid
    | UnaryDelete
    
type UpdateOperator =
    | UpdateMinus
    | UpdatePlus
        
type LogicalOperator =
    | LogicalOr
    | LogicalAnd
    
type AssignmentOperator =
    | AssignEqual
    | AssignMinus
    | AssignPlus
    | AssignMultiply
    | AssignDivide
    | AssignModulus
    | AssignShiftLeft
    | AssignShiftRightSignPropagating
    | AssignShiftRightZeroFill
    | AssignOrBitwise
    | AssignXorBitwise
    | AssignAndBitwise    
*)
(*
/// The type field is a string representing the AST variant type. 
/// Each subtype of Node is documented below with the specific string of its type field. 
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node. 
/// If the node contains no information about the source location, the field is null; 
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region) 
/// and an end position (the position of the first character after the parsed source region):
[<AbstractClass>]
type Node(typ, ?loc) =
    member x.``type``: string = typ
    member x.loc: SourceLocation option = loc

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
[<AbstractClass>] type Expression(typ, ?loc) = inherit Node(typ, ?loc = loc)

[<AbstractClass>] type Literal(typ, ?loc) = inherit Expression(typ, ?loc = loc)

[<AbstractClass>] type Statement(typ, ?loc) = inherit Node(typ, ?loc = loc)

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
[<AbstractClass>] type Declaration(typ, ?loc) = inherit Statement(typ, ?loc = loc)

/// A module import or export declaration.
[<AbstractClass>] type ModuleDeclaration(typ, ?loc) = inherit Node(typ, ?loc = loc)

type Pattern = interface end

/// Placeholder, doesn't belong to Babel specs
type EmptyExpression() =
    inherit Expression("EmptyExpression")

/// Not in Babel specs, disguised as StringLiteral    
type MacroExpression(value, args, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member x.value: string = value
    member x.args: Node[] = args
    member x.macro = true

(** ##Template Literals *)
type TemplateElement(value: string, tail, ?loc) =
    inherit Node("TemplateElement", ?loc = loc)
    member x.tail: bool = tail
    member x.value = dict [ ("raw", value); ("cooked", value) ]

type TemplateLiteral(quasis, expressions, ?loc) =
    inherit Literal("TemplateLiteral", ?loc = loc)
    member x.quasis: TemplateElement[] = quasis
    member x.expressions: Expression[] = expressions
    
type TaggedTemplateExpression(tag, quasi, ?loc) =
    inherit Expression("TaggedTemplateExpression", ?loc = loc)
    member x.tag: Expression = tag
    member x.quasi: TemplateLiteral = quasi

(** ##Identifier *)
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(name, ?loc) =
    inherit Expression("Identifier", ?loc = loc)
    member x.name: string = name
    interface Pattern
    override x.ToString() = x.name

(** ##Literals *)
type RegExpLiteral(pattern, flags, ?loc) =
    inherit Literal("RegExpLiteral", ?loc = loc)
    member x.pattern: string = pattern
    member x.flags =
        flags |> Seq.map (function
            | RegexGlobal -> "g"
            | RegexIgnoreCase -> "i"
            | RegexMultiline -> "m"
            | RegexSticky -> "y") |> Seq.fold (+) ""

type NullLiteral(?loc) =
    inherit Literal("NullLiteral", ?loc = loc)

type StringLiteral(value, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member x.value: string = value

type BooleanLiteral(value, ?loc) =
    inherit Literal("BooleanLiteral", ?loc = loc)
    member x.value: bool = value

type NumericLiteral(value, ?loc) =
    inherit Literal("NumericLiteral", ?loc = loc)
    member x.value: U2<int, float> = value    

(** ##Misc *)
type Decorator(value, ?loc) =
    inherit Node("Decorator", ?loc = loc)
    member x.value = value
    
type DirectiveLiteral(?loc) =
    inherit StringLiteral("DirectiveLiteral", ?loc = loc)

/// e.g. "use strict";
type Directive(value, ?loc) =
    inherit Node("Directive", ?loc = loc)
    member x.value: DirectiveLiteral = value    

(** ##Program *)
/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module. 
/// Otherwise, sourceType must be "script".
type Program(fileName, originalFileName, loc, body, ?directives) =
    inherit Node("Program", loc)
    member x.sourceType = "module" // Don't use "script"
    member x.body: U2<Statement, ModuleDeclaration>[] = body
    member x.directives: Directive[] = defaultArg directives [||]
    // Properties below don't belong to babel specs
    member x.fileName: string = fileName
    member x.originalFileName: string = originalFileName

(** ##Statements *)
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(expression, ?loc) =
    inherit Statement("ExpressionStatement", ?loc = loc)
    member x.expression: Expression = expression

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(body, ?directives, ?loc) =
    inherit Statement("BlockStatement", ?loc = loc)
    member x.body: Statement[] = body
    member x.directives: Directive[] = defaultArg directives [||]

/// An empty statement, i.e., a solitary semicolon.
type EmptyStatement(?loc) =
    inherit Statement("EmptyStatement", ?loc = loc)

type DebuggerStatement(?loc) =
    inherit Statement("DebuggerStatement", ?loc = loc)
    
// type WithStatement

(** ##Control Flow *)
type ReturnStatement(argument, ?loc) =
    inherit Statement("ReturnStatement", ?loc = loc)
    member x.argument: Expression = argument

// type LabeledStatement
// type BreakStatement
// type ContinueStatement

(** ##Choice *)
type IfStatement(test, consequent, ?alternate, ?loc) =
    inherit Statement("IfStatement", ?loc = loc)
    member x.test: Expression = test
    member x.consequent: Statement = consequent
    member x.alternate: Statement option = alternate    

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase(consequent, ?test, ?loc) =
    inherit Node("SwitchCase", ?loc = loc)
    member x.test: Expression option = test
    member x.consequent: Statement[] = consequent

type SwitchStatement(discriminant, cases, ?loc) =
    inherit Statement("SwitchStatement", ?loc = loc)
    member x.discriminant: Expression = discriminant
    member x.cases: SwitchCase[] = cases

(** ##Exceptions *)
type ThrowStatement(argument, ?loc) =
    inherit Statement("ThrowStatement", ?loc = loc)
    member x.argument: Expression = argument

/// A catch clause following a try block.
type CatchClause(param, body, ?loc) =
    inherit Node("CatchClause", ?loc = loc)
    member x.param: Pattern = param
    member x.body: BlockStatement = body

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(block, ?handler, ?finalizer, ?loc) =
    inherit Statement("TryStatement", ?loc = loc)
    member x.block: BlockStatement = block
    member x.handler: CatchClause option = handler
    member x.finalizer: BlockStatement option = finalizer

(** ##Declarations *)
type VariableDeclarator(id, ?init, ?loc) =
    inherit Declaration("VariableDeclarator", ?loc = loc)
    member x.id: Pattern = id
    member x.init: Expression option = init

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(kind, declarations, ?loc) =
    inherit Declaration("VariableDeclaration", ?loc = loc)
    new (var, ?init, ?loc) =
        VariableDeclaration(Var, [|VariableDeclarator(var, ?init=init, ?loc=loc)|], ?loc=loc)
    member x.declarations: VariableDeclarator[] = declarations
    member x.kind =
        match kind with Var -> "var" | Let -> "let" | Const -> "const"

(** ##Loops *)
type WhileStatement(test, body, ?loc) =
    inherit Statement("WhileStatement", ?loc = loc)
    member x.test: Expression = test
    member x.body: BlockStatement = body

type DoWhileStatement(body, test, ?loc) =
    inherit Statement("DoWhileStatement", ?loc = loc)
    member x.body: BlockStatement = body
    member x.test: Expression = test

type ForStatement(body, ?init, ?test, ?update, ?loc) =
    inherit Statement("ForStatement", ?loc = loc)
    member x.body: BlockStatement = body
    member x.init: U2<VariableDeclaration, Expression> option = init
    member x.test: Expression option = test
    member x.update: Expression option = update

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForInStatement(left, right, body, ?loc) =
    inherit Statement("ForInStatement", ?loc = loc)
    member x.body: BlockStatement = body
    member x.left: U2<VariableDeclaration, Expression> = left
    member x.right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForOfStatement(left, right, body, ?loc) =
    inherit Statement("ForOfStatement", ?loc = loc)
    member x.body: BlockStatement = body
    member x.left: U2<VariableDeclaration, Expression> = left
    member x.right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(id, args, body, ?generator, ?async, ?loc) =
    inherit Declaration("FunctionDeclaration", ?loc = loc)
    member x.id: Identifier = id
    member x.``params``: Pattern[] = args
    member x.body: BlockStatement = body
    member x.generator = defaultArg generator false
    member x.async = defaultArg async false

(** ##Expressions *)

/// A super pseudo-expression.
type Super(?loc) =
    inherit Expression("Super", ?loc = loc)

type ThisExpression(?loc) =
    inherit Expression("ThisExpression", ?loc = loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(args, body, isExpression, ?async, ?loc) =
    inherit Expression("ArrowFunctionExpression", ?loc = loc)
    member x.expression = isExpression
    member x.``params``: Pattern[] = args
    member x.body: U2<BlockStatement, Expression> = body
    member x.async: bool = defaultArg async false
        
type FunctionExpression(args, body, ?generator, ?async, ?id, ?loc) =
    inherit Expression("FunctionExpression", ?loc = loc)
    member x.id: Identifier option = id
    member x.``params``: Pattern[] = args
    member x.body: BlockStatement = body
    member x.generator: bool = defaultArg generator false
    member x.async: bool = defaultArg async false
    
/// e.g., x = do { var t = f(); t * t + 1 };
/// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
/// Doesn't seem to work well with block-scoped variables (let, const)
type DoExpression(body, ?loc) =
    inherit Expression("DoExpression", ?loc = loc)
    member x.body: BlockStatement = body
    
type YieldExpression(argument, ``delegate``, ?loc) =
    inherit Expression("YieldExpression", ?loc = loc)
    member x.argument: Expression option = argument
    /// Delegates to another generator? (yield* )
    member x.``delegate``: bool = ``delegate``    

type AwaitExpression(argument, ?loc) =
    inherit Expression("AwaitExpression", ?loc = loc)
    member x.argument: Expression option = argument

type RestProperty(argument, ?loc) =
    inherit Node("RestProperty", ?loc = loc)
    member x.argument: Expression = argument

/// e.g., var z = { x: 1, ...y } // Copy all properties from y 
type SpreadProperty(argument, ?loc) =
    inherit Node("SpreadProperty", ?loc = loc)
    member x.argument: Expression = argument

type SpreadElement(argument, ?loc) =
    inherit Node("SpreadElement", ?loc = loc)
    member x.argument: Expression = argument
    
type ArrayExpression(elements, ?loc) =
    inherit Expression("ArrayExpression", ?loc = loc)
    member x.elements: U2<Expression, SpreadElement> option[] = elements

[<AbstractClass>]
type ObjectMember(typ, key, ?value, ?computed, ?loc) =
    inherit Node(typ, ?loc = loc)
    member x.key: Expression = key
    member x.value: Expression option = value
    member x.computed: bool = defaultArg computed false
    // member x.decorators: Decorator[] = defaultArg decorators []
    
type ObjectProperty(key, value, ?shorthand, ?computed, ?loc) =
    inherit ObjectMember("ObjectProperty", key, value, ?computed=computed, ?loc=loc)
    member x.shorthand: bool = defaultArg shorthand false

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod(kind, key, args, body, ?computed, ?generator, ?async, ?loc) =
    inherit ObjectMember("ObjectMethod", key, ?computed=computed, ?loc=loc)
    member x.kind = match kind with ObjectGetter -> "get"
                                  | ObjectSetter -> "set"
                                  | ObjectMeth -> "method"
    member x.``params``: Pattern[] = args
    member x.body: BlockStatement = body
    member x.generator: bool = defaultArg generator false
    member x.async: bool = defaultArg async false

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression. 
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(``object``, property, ?computed, ?loc) =
    inherit Expression("MemberExpression", ?loc = loc)
    member x.``object``: Expression = ``object``
    member x.property: Expression = property
    member x.computed: bool = defaultArg computed false
    interface Pattern

type ObjectExpression(properties, ?loc) =
    inherit Expression("ObjectExpression", ?loc = loc)
    member x.properties: U3<ObjectProperty, ObjectMethod, SpreadProperty>[] = properties

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(test, consequent, alternate, ?loc) =
    inherit Expression("ConditionalExpression", ?loc = loc)
    member x.test: Expression = test
    member x.consequent: Expression = consequent
    member x.alternate: Expression = alternate

/// A function or method call expression.  
type CallExpression(callee, args, ?loc) =
    inherit Expression("CallExpression", ?loc = loc)
    member x.callee: Expression = callee
    member x.arguments: U2<Expression, SpreadElement>[] = args

type NewExpression(callee, args, ?loc) =
    inherit Expression("NewExpression", ?loc = loc)
    member x.callee: Expression = callee
    member x.arguments: U2<Expression, SpreadElement>[] = args

/// A comma-separated sequence of expressions.
type SequenceExpression(expressions, ?loc) =
    inherit Expression("SequenceExpression", ?loc = loc)
    member x.expressions: Expression[] = expressions

(** ##Unary Operations *)
type UnaryExpression(operator, argument, ?prefix, ?loc) =
    inherit Expression("UnaryExpression", ?loc = loc)
    member x.prefix: bool = defaultArg prefix true
    member x.argument: Expression = argument
    member x.operator =
        match operator with
        | UnaryMinus -> "-"
        | UnaryPlus -> "+"
        | UnaryNot -> "!"
        | UnaryNotBitwise -> "~"
        | UnaryTypeof -> "typeof"
        | UnaryVoid -> "void"
        | UnaryDelete -> "delete"           

type UpdateExpression(operator, prefix, argument, ?loc) =
    inherit Expression("UpdateExpression", ?loc = loc)
    member x.prefix: bool = prefix
    member x.argument: Expression = argument
    member x.operator =
        match operator with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"
    

type AssignmentExpression(operator, left, right, ?loc) =
    inherit Expression("AssignmentExpression", ?loc = loc)
    member x.left: Expression = left
    member x.right: Expression = right
    member x.operator =
        match operator with    
        | AssignEqual -> "="
        | AssignMinus -> "-="
        | AssignPlus -> "+="
        | AssignMultiply -> "*="
        | AssignDivide -> "/="
        | AssignModulus -> "%="
        | AssignShiftLeft -> "<<="
        | AssignShiftRightSignPropagating -> ">>="
        | AssignShiftRightZeroFill -> ">>>="
        | AssignOrBitwise -> "|="
        | AssignXorBitwise -> "^="
        | AssignAndBitwise -> "&="
    
type LogicalExpression(operator, left, right, ?loc) =
    inherit Expression("LogicalExpression", ?loc = loc)
    member x.left: Expression = left
    member x.right: Expression = right
    member x.operator =
        match operator with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"
        

(** ##Patterns *)
// type AssignmentProperty(key, value, ?loc) =
//     inherit ObjectProperty("AssignmentProperty", ?loc = loc)
//     member x.value: Pattern = value

// type ObjectPattern(properties, ?loc) =
//     inherit Node("ObjectPattern", ?loc = loc)
//     member x.properties: U2<AssignmentProperty, RestProperty>[] = properties
//     interface Pattern

type ArrayPattern(elements, ?loc) =
    inherit Node("ArrayPattern", ?loc = loc)
    member x.elements: Pattern option[] = elements
    interface Pattern

type AssignmentPattern(left, right, ?loc) =
    inherit Node("AssignmentPattern", ?loc = loc)
    member x.left: Pattern = left
    member x.right: Expression = right
    interface Pattern

type RestElement(argument, ?loc) =
    inherit Node("RestElement", ?loc = loc)
    member x.argument: Pattern = argument
    interface Pattern        

(** ##Classes *)
type ClassMethodKind =
    | ClassConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(kind, key, args, body, computed, ``static``, ?loc) =
    inherit Node("ClassMethod", ?loc = loc)
    member x.kind = match kind with ClassConstructor -> "constructor"
                                  | ClassGetter -> "get"
                                  | ClassSetter -> "set"
                                  | ClassFunction -> "method"
    member x.key: Expression = key
    member x.``params``: Pattern[] = args
    member x.body: BlockStatement = body
    member x.computed: bool = computed
    member x.``static``: bool = ``static``
    // member x.decorators: Decorator[] = defaultArg decorators []
    // This appears in astexplorer.net but it's not documented
    // member x.expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(key, value, ?loc) =
    inherit Node("ClassProperty", ?loc = loc)
    member x.key: Identifier = key
    member x.value: Expression = value

type ClassBody(body, ?loc) =
    inherit Node("ClassBody", ?loc = loc)
    member x.body: U2<ClassMethod, ClassProperty>[] = body

type ClassDeclaration(body, id, ?super, ?loc) =
    inherit Declaration("ClassDeclaration", ?loc = loc)
    member x.body: ClassBody = body
    member x.id: Identifier = id
    member x.superClass: Expression option = super
    // member x.decorators: Decorator[] = defaultArg decorators []

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(body, ?id, ?super, ?loc) =
    inherit Expression("ClassExpression", ?loc = loc)
    member x.body: ClassBody = body
    member x.id: Identifier option = id    
    member x.superClass: Expression option = super
    // member x.decorators: Decorator[] = defaultArg decorators []

// type MetaProperty(meta, property, ?loc) =
//     inherit Expression("MetaProperty", ?loc = loc)
//     member x.meta: Identifier = meta
//     member x.property: Expression = property

(** ##Modules *)
/// A specifier in an import or export declaration.
[<AbstractClass>]
type ModuleSpecifier(typ, local, ?loc) =
    inherit Node(typ, ?loc = loc)
    member x.local: Identifier = local

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod". 
/// The imported field refers to the name of the export imported from the module. 
/// The local field refers to the binding imported into the local module scope. 
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo. 
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier(local, imported, ?loc) =
    inherit ModuleSpecifier("ImportSpecifier", local, ?loc = loc)
    member x.imported: Identifier = imported

/// A default import specifier, e.g., foo in import foo from "mod".
type ImportDefaultSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportDefaultSpecifier", local, ?loc = loc)
    
/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportNamespaceSpecifier", local, ?loc = loc)

/// e.g., import foo from "mod";.
type ImportDeclaration(specifiers, source, ?loc) =
    inherit ModuleDeclaration("ImportDeclaration", ?loc = loc)
    member x.specifiers: U3<ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier>[] = specifiers
    member x.source: Literal = source

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}. 
/// The exported field refers to the name exported in the module. 
/// The local field refers to the binding into the local module scope. 
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes; 
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo}, 
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(local, exported, ?loc) =
    inherit ModuleSpecifier("ExportSpecifier", local, ?loc = loc)
    member x.exported: Identifier = exported
    
/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(?declaration, ?specifiers, ?source, ?loc) =
    inherit ModuleDeclaration("ExportNamedDeclaration", ?loc = loc)
    member x.declaration: Declaration option = declaration
    member x.specifiers: ExportSpecifier[] = defaultArg specifiers [||]
    member x.source: Literal option = source

/// An export default declaration, e.g., export default function () {}; or export default 1;. 
type ExportDefaultDeclaration(declaration, ?loc) =
    inherit ModuleDeclaration("ExportDefaultDeclaration", ?loc = loc)
    member x.declaration: U2<Declaration, Expression> = declaration

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(source, ?loc) =
    inherit ModuleDeclaration("ExportAllDeclaration", ?loc = loc)
    member x.source: Literal = source
*)