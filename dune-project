(lang dune 3.2)
(using mdx 0.2)
(name hedgehog)

(generate_opam_files true)

(license BSD-3-Clause)
(authors "Tim McGilchrist <timmcgil@gmail.com>")
(maintainers "Tim McGilchrist <timmcgil@gmail.com>")
(source (github tmcgilchrist/ocaml-hedgehog))

(package
 (name hedgehog)
 (synopsis "Property-based testing with integrated shrinking for OCaml")
 (description "Hedgehog is a property-based testing library with integrated shrinking, using OCaml 5 effects for assertions and logging.")
 (depends
  (ocaml (>= 5.1))
  (domainslib (>= 0.5))
  (ocamlformat (and (= 0.29.0) :with-dev-setup))
  (odoc (and (>= 3.2.1) :with-doc))
  (mdx :with-test)))

(package
 (name hedgehog-alcotest)
 (synopsis "Alcotest integration for Hedgehog property-based testing")
 (description "Run Hedgehog properties as Alcotest test cases.")
 (depends
  (ocaml (>= 5.1))
  (hedgehog (= :version))
  (alcotest (>= 1.0))))
