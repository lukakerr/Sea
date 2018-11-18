# Sea

A simple evaluator for a C like language, currently a work in progress.
Built to learn Haskell and the process of writing a lexer, parser and evaluator.

### Running

```bash
$ stack build

# Run the evaluator
$ stack run sea tests/evaluator/2.sea

# Run the lexer
$ stack run sea tests/evaluator/2.sea --lexer

# Run the parser
$ stack run sea tests/evaluator/2.sea --parser
```

### Testing

```bash
$ ./test.sh
```

### Examples

Below are some example `.sea` programs to demonstrate the syntax.

```assembly
;; types
bln var = true
num x = 5
str string = 'Hello'
str stringTwo = "Hello again"

;; functions
num factorial {num n} (
  if {n < 0} run (
    ret -1
  ) else (
    num r = 1

    while {n > 0} (
      r *= n
      n -= 1
    )

    ret r
  )
)
```
