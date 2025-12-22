Camelotte
=========

(Archaic) Spelling variant of [camelote][wiktionary]: poor quality goods; junk,
rubbish, tat

[wiktionary]: https://en.wiktionary.org/wiki/camelote

Monorepo of small OCaml libraries and programs of questionable quality...
but maintained!

## Content

You can also visit the [API documentation] for these packages.

[API documentation]: https://lesboloss-es.github.io/Camelotte/

- [Monadise](monadise/) — Monadise direct-style functions easily
- [Next](next/) — Niols's extended standard library
- [ppx_deriving_biniou](ppx_deriving_biniou/) — Derive conversion functions
  between OCaml types and Biniou trees
- [ppx_deriving_madcast](ppx_deriving_madcast/) — Derive cast functions from
  their types
- [ppx_monad](ppx_monad/) — Syntax extension for all monadic syntaxes
- [SpacedOut](spacedout/) — Helpers to read and write space-separated data files
- [TestU01](testu01/) — Bindings for [TestU01](https://simul.iro.umontreal.ca/testu01/tu01.html),
  “a collection of utilities for the empirical statistical testing of uniform random number generators.”
- [Valet](valet/) — Declare types from `let`-bindings with a preceeding `val`
  declaration

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

The CI pushes the built derivations to a Nix cache, which Nix users might enjoy
to use. The flake adds it as an `extra-trusted-substituters`.

The Nix environment contains Git hooks that will check formatting of OCaml with
Topiary, for instance, or that `dune-project` and the `*.opam` files are in
sync. This is also enforced by the CI. There is currently no good way to follow
this the OPAM way.

## Importing

Say you have a project `<proj>` that lives in its own repository `<repo>` and
that should come to live in Camelotte under `<dir>/`:

- Rewrite its history to have it live in a subdirectory instead:
  ```
  git clone <repo> <proj>
  cd <proj>
  git filter-repo --to-subdirectory-filter <dir>/
  ```
  Note: `git filter-repo` can crash monumentally on some Git configurations;
  you might want to `mv ~/.config/git/config{,.bak}` during the rewrite.

- Import the rewritten repository into Camelotte:
  ```
  cd camelotte
  git remote add import /path/to/<proj>
  git fetch import
  git merge --allow-unrelated-histories import/main
  ## => use “Import history from `<proj>`” as commit message
  git remote remove import
  ```

- Then comes the work of integrating the project with the monorepo, adding it to
  this README, to `dune-project` and `flake.nix`, importing or cleaning up
  information from its `.gitignore`, `.github`, etc. and potentially renaming
  its tags and recreating its releases.
