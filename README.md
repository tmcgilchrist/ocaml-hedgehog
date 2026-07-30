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
- Small footprint: the OCaml >= 5.1 stdlib plus `domainslib` for parallel checking.

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

## How It Works

Hedgehog is built on integrated shrinking, the same foundation QCheck2
adopted in 2021:

- **Integrated shrinking via rose trees** — `Gen.t = int -> Seed.t -> 'a Tree.t option`. Every generator produces a rose tree where the root is the generated value and children are shrunk alternatives, so shrinking always respects generator invariants (filters, preconditions) and no manual shrink functions are needed. QCheck2 works the same way; QCheck's original API and JS Quickcheck keep generators and shrinkers separate.
- **Recursive tree binding** — `Gen.bind` recursively binds through the entire shrink tree, matching Haskell Hedgehog's `TreeT` monad semantics. Composed generators (`let*`) automatically compose their shrinking.
- **Subterm combinators** — `Gen.subterm`, `subterm2`, `subterm3` shrink recursive structures (ASTs, expressions) by trying subterms directly before shrinking within the constructor. Neither QCheck nor JS Quickcheck has an equivalent.
- **Range-controlled generation** — `Range.t` is a reusable value pairing an origin with size-dependent bounds, so `Range.linear 0 100` scales with the size parameter and shrinks toward the origin wherever it is used. QCheck2 has the pieces separately (`Gen.int_range ?origin`, `Gen.sized`) but no combined abstraction. Golden ratio scaling in `Gen.recursive` naturally controls recursion depth.
- **Algebraic effects** — `assert_`, `annotate`, `cover`, `classify` use `Effect.perform`, keeping test logic cleanly separated from generators. QCheck returns test outcomes as values; JS Quickcheck uses exceptions and integrates with Expect_test.
- **Coverage enforcement** — `cover 50.0 "positive" (n > 0)` fails the property if fewer than 50% of test cases satisfy the condition. QCheck reports distributions via `?collect` and `?stats` but cannot enforce a threshold; JS Quickcheck has no equivalent.
- **State machine testing in the library** — `Stm` supports sequential and parallel (linearizability) checking with `Domain.spawn`. QCheck offers this through the separate `qcheck-stm` and `qcheck-lin` packages, which pioneered the approach for OCaml 5.
- **Parallel property runner** — `Property.check_parallel` runs a group's properties concurrently across domains via `domainslib`. QCheck's runners are sequential.
- **LCS diff on failure** — `===` and `diff` assertions produce line-level diffs between expected and actual values.
- **Small dependency footprint** — the stdlib plus `domainslib`. `qcheck-core` needs only `unix`; `base_quickcheck` pulls in `base`, `ppxlib` and several `ppx_*` libraries.

What Hedgehog lacks:
 1. **ppx derivation** (QCheck has `ppx_deriving_qcheck`, JS Quickcheck `[%quickcheck.generator: int list]`) and
 2. **function generation** (QCheck's `Observable`/`Fn`, JS Quickcheck's `Observer.t`).

Both require writing generators explicitly.

See the [Alternatives](https://tmcgilchrist.github.io/ocaml-hedgehog/guides/alternatives/) page for a detailed comparison.

## Architecture

| Module              | Description                                                        |
|---------------------|--------------------------------------------------------------------|
| `Hedgehog.Seed`     | Splittable PRNG built on OCaml 5's `Random.State`                  |
| `Hedgehog.Tree`     | Rose tree with lazy children for integrated shrinking              |
| `Hedgehog.Shrink`   | Pure shrinking strategies (binary search, halving, list removal)   |
| `Hedgehog.Range`    | Size-dependent ranges (constant, linear, exponential)              |
| `Hedgehog.Gen`      | Generator monad with numeric, string, list, and choice combinators |
| `Hedgehog.Property` | Property runner with OCaml 5 effect-based assertions               |
| `Hedgehog.Stm`      | State machine testing with sequential and parallel checking        |

## Documentation

Full documentation is available at https://tmcgilchrist.github.io/ocaml-hedgehog/.

To build and preview the docs site locally:

```shell
opam install odoc          # one-time: needs odoc >= 3.2.1
cd website && npm install  # one-time: install Astro/Starlight
make website-dev           # generate markdown, then start the dev server
```

The site will be available at `http://localhost:4321/ocaml-hedgehog/`.

The documentation source lives in `doc/*.mld` files. These are processed
by odoc's markdown backend into Starlight-compatible markdown, then built
into a static site with Astro. Generating the markdown needs odoc >= 3.2.1
and dune >= 3.22 (which added the `@doc-markdown` alias); building and
testing the library itself has no such requirement.

## Resources

- [haskell-hedgehog](https://github.com/hedgehogqa/haskell-hedgehog) — The original Haskell implementation.
- [Fast Splittable Pseudorandom Number Generators](https://doi.org/10.1145/2660193.2660195) — Steele et al., on the design of splittable PRNGs.

 [github-shield]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml/badge.svg
 [github-ci]: https://github.com/tmcgilchrist/ocaml-hedgehog/actions/workflows/ci.yml
