ocaml-jack - Jack is an alternative to QuickCheck generators / shrinking.
=========================================================================

Under development come back later.

Setup OCaml compiler

``` shell
opam switch install 4.03.0-jack --alias-of 4.03.0
eval `opam config env`
```

Install dependencies

``` shell
jbuilder external-lib-deps --missing @runtest
# prints out the opam command to install everything
```

Build

``` shell
jbuilder build
```

Run tests

``` shell
jbuilder runtest
```


Resources
=========================================================================

PRINGO: Pseudo-Random, splittable Number Generators for Ocaml https://github.com/xavierleroy/pringo/