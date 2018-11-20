# Sea

A simple evaluator for a C like language, currently a work in progress.
Built to learn Haskell and the process of writing a lexer, parser and evaluator.

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

# Sea> print 1 + 2
# 3
# Sea> num n = 5 ret n
# 5
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
  num n = 5
  ret n * 2
)
```
