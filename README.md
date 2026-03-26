<div align="center">

# hedgehog-ocaml

### Release with confidence.

[![GitHub CI][github-shield]][github-ci]

<div align="left">

[Hedgehog](https://github.com/tmcgilchrist/ocaml-hedgehog) automatically generates a comprehensive array of test cases, exercising your software in ways human testers would never imagine.

Generate hundreds of test cases automatically, exposing even the most insidious of corner cases. Failures are automatically simplified, giving developers coherent, intelligible error messages.

## Features

- Integrated shrinking, shrinks obey invariants by construction.
- Generators produce rose trees where the root is the generated value and children are shrunk alternatives.
- Range combinators for full control over the scope of generated numbers and collections.
- Equality and diff assertions with OCaml 5 algebraic effects.
- Monadic generator composition with `let*` / `let+` / `and+` binding operators.
- `and+` enables parallel shrinking via `Tree.mzip`.
- Zero dependencies beyond OCaml >= 5.0.

## Example

The main module, `Hedgehog`, includes everything you need to get started
writing property tests.

```ocaml
open Hedgehog
```

Once you have your imports set up, you can write a simple property:

```ocaml
let prop_reverse =
  Property.property Gen.(
    let* xs = list (Range.linear 0 100) alpha in
    return (fun () ->
      Property.assert_ (List.rev (List.rev xs) = xs)))
```

You can then run it:

```ocaml
let () =
  if Property.check prop_reverse then
    print_endline "All tests passed."
  else
    exit 1
```

```
+++ OK, passed 100 tests.
All tests passed.
```

When a property fails, Hedgehog automatically finds a minimal
counterexample:

```ocaml
let prop_bad =
  Property.property Gen.(
    let* n = int (Range.linear 0 1000) in
    return (fun () ->
      Property.annotate (Printf.sprintf "n = %d" n);
      Property.assert_ (n < 500)))
```

```
*** Failed! Falsifiable (after 17 tests):
  n = 500
  Assertion failed
```

## Building

```shell
opam install . --deps-only --with-test
dune build
```

## Running Tests

```shell
dune runtest
```

## Architecture

```
Seed ──┐
       │
Tree ──┤
       ├──► Gen ──► Property
Shrink─┤
       │
Range ──┘
```

| Module | Description |
|--------|-------------|
| `Hedgehog.Seed` | SplitMix64 splittable PRNG |
| `Hedgehog.Tree` | Rose tree with lazy children for integrated shrinking |
| `Hedgehog.Shrink` | Pure shrinking strategies (binary search, halving, list removal) |
| `Hedgehog.Range` | Size-dependent ranges (constant, linear, exponential) |
| `Hedgehog.Gen` | Generator monad with numeric, string, list, and choice combinators |
| `Hedgehog.Property` | Property runner with OCaml 5 effect-based assertions |

## Resources

- [haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) — The original Haskell implementation.
- [Fast Splittable Pseudorandom Number Generators](https://doi.org/10.1145/2660193.2660195) — Steele et al., the paper behind the SplitMix PRNG.

 [github-shield]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml
