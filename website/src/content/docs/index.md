---
title: Hedgehog
description: Property-based testing with integrated shrinking for OCaml
draft: false
template: splash
hero:
  tagline: Property-based testing with integrated shrinking for OCaml
  actions:
    - text: Get Started
      link: /ocaml-hedgehog/guides/getting-started/
      icon: right-arrow
      variant: primary
    - text: View on GitHub
      link: https://github.com/tmcgilchrist/ocaml-hedgehog
      icon: external
---

## Features

- **Integrated Shrinking** — Generators produce rose trees of outcomes. Every generator shrinks for free — no manual shrink functions needed.
- **Range-Controlled Generation** — Numeric ranges separate bounds from origin, giving precise control over value distribution and shrink direction.
- **Effects-Based Assertions** — OCaml 5 algebraic effects keep generators and test logic cleanly separated. Use `assert_`, `annotate`, `cover` and more.
- **State Machine Testing** — Test stateful systems against abstract models with sequential and parallel linearizability checking.

## Quick start

```
opam install hedgehog
```

```ocaml
open Hedgehog

let () =
  Property.check
    Property.(property Gen.(
      let* xs = list (Range.linear 0 100) (int (Range.linear 0 1000)) in
      return (fun () ->
        assert_ (List.rev (List.rev xs) = xs))))
  |> ignore
```
