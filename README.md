# Sea

A simple evaluator for a C like language, currently a work in progress.

Built to learn Haskell and the process of writing a lexer, parser and evaluator.

A recursive descent parser is used for parsing.

Currently in the very early stages of development.

### Running

First build `sea`

```bash
$ stack build
```

You can write a `.sea` program and evaluate, lex or parse it:

```bash
# Run the evaluator
$ stack run sea tests/evaluator/2.sea

# Run the lexer
$ stack run sea tests/evaluator/2.sea --lexer

# Run the parser
$ stack run sea tests/evaluator/2.sea --parser
```

Or you can run the interactive REPL:

```bash
$ stack run sea repl

# Sea> 1 + 2
# 3
# Sea> str n = "Hello Sea" n
# Hello Sea
# Sea> :q
```

### Testing

```bash
$ ./test.sh
```

### Examples

Below is an example `.sea` program to demonstrate the syntax.
This will be updated as the parser and evaluator progress.

```assembly
;; sea only supports a single main {} function currently
fn main {} (
  num n = 3 * 4
  str s = "Hello world"
  str ss = s + " again"

  if {n > 10} (
    ret ss
  ) else (
    ret s
  )
)
```
