Camelotte
=========

(Archaic) Spelling variant of [camelote][wiktionary]: poor quality goods; junk,
rubbish, tat

[wiktionary]: https://en.wiktionary.org/wiki/camelote

Monorepo of small OCaml libraries and programs of questionable quality... 
but maintained!

## Content

- [Monadise](monadise/) — Monadise direct-style functions easily
- [Next](next/) — Niols's extended standard library
- [ppx_monad](ppx_monad/) — Syntax extension for all monadic syntaxes
- [SpacedOut](spacedout/) — Helpers to read and write space-separated data files
- [Valet](valet/) — Preprocessor allowing to declare types from `let`-bindings
  with a preceeding `val` declaration

## License

The content of this repository, unless stated otherwise, is licensed under the
MIT license. See [LICENSE.md](./LICENSE.md).

## Contributing

The CI checks periodically that everything builds via Nix or via the OPAM
packaging. You may also choose either of these environments to contribute to
this repository:

- The Nix way: the flake provides a devShell for all your needs: `nix develop`,
  or `echo 'use flake' > .envrc`.

- The OPAM way: `opam install . --deps-only --with-test --with-doc`.

The Nix environment contains Git hooks that will check formatting of OCaml with
Topiary, for instance, or that `dune-project` and the `*.opam` files are in
sync. This is also enforced by the CI. There is currently no good way to follow
this the OPAM way.
