# mexpr

A small toy expression-language interpreter, including a hand-written lexer and parser, evaluation, functions and recursive bindings, mutable references, and Hindley–Milner type inference with polymorphic `let` bindings.

```sh
# Start the REPL
dune exec mexpr

# Run an example
dune exec mexpr -- examples/fibonacci.mx

# Run the tests
dune runtest
```

Requires OCaml and Dune 3.23 or newer.
