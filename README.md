# Sea

A simple evaluator for a C like language, currently a work in progress.
Built to learn Haskell and the process of writing an evaluator.

### Running

```bash
$ stack build
$ stack run sea tests/1.sea
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
