# Mini DSL Specification

This repository experiments with a deliberately tiny expression language that
still provides the pieces required by the README plan: tokens, parser,
typechecker, and CLI driver. The surface syntax intentionally mirrors Rust and
ML influences so it is ergonomic to read.

## Lexical structure

* ASCII identifiers start with `_` or an alphabetic character, with alphanumeric
  or `_` trailing characters.
* Numeric literals are base-10 `i32` integers.
* Boolean literals are `true` and `false`.
* Keywords: `fn`, `let`, `if`, `then`, `else`, `i32`, `bool`, `unit`.
* Symbols: `(` `)` `{` `}` `,` `:` `;` `->` `=` `==` `<` `>` `+` `-` `*` `/`.
* Line comments start with `//` and extend to the newline.

## Grammar (informal)

```
program      ::= item*
item         ::= "fn" IDENT "(" param_list? ")" "->" type block
param_list   ::= param ("," param)*
param        ::= IDENT ":" type

block        ::= "{" block_item* "}"
block_item   ::= let_stmt ";"?
               | expr (";" if not followed by "}")
let_stmt     ::= "let" IDENT (":" type)? "=" expr

expr         ::= if_expr
if_expr      ::= "if" expr "then" expr "else" expr
               | equality

equality     ::= comparison ("==" comparison)*
comparison   ::= term (("<" | ">") term)*
term         ::= factor (("+" | "-") factor)*
factor       ::= call (("*" | "/") call)*
call         ::= primary ("(" arg_list? ")")*
arg_list     ::= expr ("," expr)*
primary      ::= INT | BOOL | IDENT | block | "(" expr ")"

type         ::= "i32" | "bool" | "unit"
```

Blocks evaluate to the last contained expression and introduce a new lexical
scope. `let` bindings are scoped to the smallest enclosing block.

## Type system

* Primitive types: `i32`, `bool`, `unit`.
* Function types: `fn(param_0, ..., param_n) -> ret` automatically derived from
  the parameter list and function return annotation.
* Arithmetic (`+ - * /`) requires `i32` operands and returns `i32`.
* Comparisons `<` and `>` require `i32` operands and return `bool`.
* Equality `==` requires both operands share the same type and returns `bool`.
* `if` expressions require a `bool` condition and both branches must have the
  same type.
* Function calls require the callee expression to have a function type and the
  argument count and types must match the signature.

The typechecker infers local variable types from the initializer expression, and
optional annotations on `let` bindings are enforced when present.

## Intermediate representations

Every successfully typechecked program flows through three progressively lower
representations before codegen. They currently mirror the surface AST but serve
as clear extension points:

* **HIR** – the “desugared” tree. Future passes will normalize `if`/`block`
  constructs and resolve syntactic sugar here.
* **MIR** – a control-flow oriented IR that will eventually expose explicit
  temporaries and structured branches.
* **LowIR** – a codegen-friendly view (today still close to MIR) that feeds the
  LLVM backend. When we add real lowering, this stage will be responsible for
  jumps, SSA temps, and function-local metadata.

Each representation exposes a `dump()` helper so tools can inspect the pipeline
without mutating upstream stages.

## CLI behaviors

* `dslc --dump-tokens` prints every token with a span.
* `dslc --dump-ast` pretty-prints the AST (`Program`).
* `dslc --emit-llvm` prints the textual LLVM IR generated from `LowIR`.
* `dslc --run` pipes the IR through `llvm-as` and runs it with `lli`, exiting
  with the program’s return code. The sample `examples/foo.dsl` returns `0` by
  default so this command succeeds out of the box.
