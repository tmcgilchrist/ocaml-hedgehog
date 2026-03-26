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
- State machine testing with sequential and parallel (linearizability) checking via `Stm`.
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

## State Machine Testing

The `Stm` module lets you test stateful systems by defining a model
specification and checking that the real implementation matches:

```ocaml
open Hedgehog

module Counter_spec = struct
  type cmd = Incr | Decr | Get
  type state = int
  type sut = int ref
  type result = Unit | Int of int

  let show_cmd = function Incr -> "Incr" | Decr -> "Decr" | Get -> "Get"
  let show_result = function Unit -> "()" | Int n -> string_of_int n
  let gen_cmd _state = Gen.element [Incr; Decr; Get]
  let shrink_cmd _ = Seq.empty

  let init_state = 0
  let init_sut () = ref 0
  let cleanup _ = ()

  let next_state cmd state = match cmd with
    | Incr -> state + 1 | Decr -> state - 1 | Get -> state

  let precond _state _cmd = true

  let run cmd sut = match cmd with
    | Incr -> incr sut; Unit
    | Decr -> decr sut; Unit
    | Get -> Int !sut

  let postcond cmd state result = match cmd, result with
    | Get, Int n -> n = state
    | (Incr | Decr), Unit -> true
    | _ -> false
end

module Counter_stm = Stm.Make(Counter_spec)
```

Run a sequential test to check postconditions at each step:

```ocaml
let () =
  if Property.check (Counter_stm.sequential ()) then
    print_endline "Sequential: OK"
```

Run a parallel test to detect concurrency bugs via linearizability checking:

```ocaml
let () =
  if Property.check (Counter_stm.parallel ()) then
    print_endline "Parallel: OK"
```

## Alcotest Integration

The `hedgehog-alcotest` package lets you run Hedgehog properties as
Alcotest test cases:

```shell
opam install hedgehog-alcotest
```

Use `Hedgehog_alcotest.to_alcotest` to wrap a property:

```ocaml
let () =
  Alcotest.run "my-tests" [
    "properties", [
      Hedgehog_alcotest.to_alcotest "reverse involution"
        Hedgehog.(Property.property Gen.(
          let* xs = list (Range.linear 0 100) alpha in
          return (fun () ->
            Property.assert_ (List.rev (List.rev xs) = xs))));

      Hedgehog_alcotest.to_alcotest "small lists"
        Hedgehog.(Property.property Gen.(
          let* xs = list (Range.linear 0 100) (int (Range.linear 0 1000)) in
          return (fun () ->
            Property.annotate (Printf.sprintf "xs has %d elements" (List.length xs));
            Property.assert_ (List.length xs < 5))));
    ]
  ]
```

Passing properties return normally. Failures call `Alcotest.fail` with
the shrunk counterexample:

```
[FAIL]  properties  1  small lists.
*** Failed! Falsifiable (after 9 tests):
  xs has 5 elements
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
       ├──► Gen ──► Property ──► Stm
Shrink─┤
       │
Range ─┘
```

| Module              | Description                                                        |
|---------------------|--------------------------------------------------------------------|
| `Hedgehog.Seed`     | SplitMix64 splittable PRNG                                         |
| `Hedgehog.Tree`     | Rose tree with lazy children for integrated shrinking              |
| `Hedgehog.Shrink`   | Pure shrinking strategies (binary search, halving, list removal)   |
| `Hedgehog.Range`    | Size-dependent ranges (constant, linear, exponential)              |
| `Hedgehog.Gen`      | Generator monad with numeric, string, list, and choice combinators |
| `Hedgehog.Property` | Property runner with OCaml 5 effect-based assertions               |
| `Hedgehog.Stm`      | State machine testing with sequential and parallel checking        |

## Resources

- [haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) — The original Haskell implementation.
- [Fast Splittable Pseudorandom Number Generators](https://doi.org/10.1145/2660193.2660195) — Steele et al., the paper behind the SplitMix PRNG.

 [github-shield]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml
