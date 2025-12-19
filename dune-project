(lang dune 3.20)

(name camelotte)

(source (github niols/camelotte))
(maintainers "Niols <niols@niols.fr>")

(license MIT)

(generate_opam_files true)

(package
 (name monadise)
 (synopsis "Monadise direct-style functions easily")
 (authors "Niols <niols@niols.fr>")
 (depends
  (ocaml (>= 5.0))
  (alcotest :with-test)
  (alcotest-lwt :with-test)
  (lwt :with-test)))

(package
 (name monadise-lwt)
 (synopsis "Monadise direct-style functions easily")
 (authors "Niols <niols@niols.fr>")
 (depends
   monadise
  (ocaml (>= 5.0))
   lwt))

(package
 (name next)
 (synopsis "Niols's extended standard library")
 (authors "Niols <niols@niols.fr>")
 (depends
  (alcotest :with-test)))

(package
 (name ppx_deriving_madcast)
 (version 0.2)
 (tags (syntax))
 (synopsis "Library deriving cast functions based on their types")
 (license LGPL-3.0-or-later)
 (authors "Kerl <kerl@wkerl.me>" "Niols <niols@niols.fr>")
 (description
  "\| This package provides a PPX that allows to derive cast functions based
  "\| on their types.
  "\|
  "\| For instance, `[%madcast: ('a * string) -> ('a * int)]` would be replaced by:
  "\|
  "\|     fun (x, y) ->
  "\|       (x,
  "\|        try int_of_string y
  "\|        with Failure _ -> failwith \"madcast: string -> int\")
 )
 (depends
  (dune          (and :build (>= "2.5")))
  (ocaml         (>= "4.05.0"))
  (odoc          :with-test)
  (ppx_deriving  (and :build (>= "5.0")))
  (ppxlib        (>= "0.15.0"))
 ))

(package
 (name ppx_monad)
 (synopsis "A Syntax Extension for all Monadic Syntaxes")
 (authors "Niols <niols@niols.fr>")
 (depends
   ocaml
   dune-configurator
   ppxlib))

(package
 (name spacedout)
 (synopsis "Helpers to read and write space-separated data files")
 (authors "Niols <niols@niols.fr>")
 (depends
  (alcotest :with-test)))

(package
 (name testu01)
 (synopsis "OCaml bindings for TestU01 (and Probdist and MyLib)")
 (authors
  "Niols <niols@niols.fr>"
  "Kerl <kerl@wkerl.me>")
 (license GPL-3.0-or-later)
 (description "This package provides OCaml bindings for TestU01 1.2.3, as well
  as Probdist and Mylib. TestU01 is a software library, implemented in C, and
  offering a collection of utilities for the empirical statistical testing of
  uniform random number generators. The OCaml bindings allow for easy testing of
  random number generators written in OCaml and that claim to be uniform.")
 (depends
  (ocaml  (>= 4.06.0))
  (md2mld (or :build :with-doc))))

(package
 (name valet)
 (synopsis "An OCaml preprocessor allowing to declare types from `let`-bindings with a preceeding `val` declaration")
 (authors "Niols <niols@niols.fr>")
 (depends
   ocaml
   ppxlib))

(formatting (enabled_for dune))
