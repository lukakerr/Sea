# Sea

A simple evaluator for a C like language, currently a work in progress.
Built to learn Haskell and the process of writing an evaluator.

```assembly
;; factorial function
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

### Running

```bash
$ stack build
$ stack run sea tests/1.sea
```

### Testing

```bash
$ ./test.sh
```
