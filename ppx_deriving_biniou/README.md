# `ppx_deriving_biniou`

A [ppx_deriving] plugin that generates conversion functions between OCaml types
and [Biniou] trees.

`ppx_deriving_biniou` provides a simple way to serialise and deserialise OCaml
types to and from the Biniou binary format. It works similarly to
`ppx_deriving_yojson` but targets Biniou instead of JSON. Biniou is a binary
data format designed for speed, safety, and ease of use. It's more compact and
faster than JSON while maintaining many of the same benefits. For more
information, see [Biniou]. The library converts OCaml values to and from
[`Bi_io.tree`], Biniou's tree representation.

See also: [Comparison to Atdgen](#comparison-to-atdgen)

## Installation

### via Nix

The library is available in the Nix flake:

```bash
nix build github:lesboloss-es/camelotte#ppx_deriving_biniou
```

### via opam pin

You can pin the library directly from the repository:

```bash
opam pin add ppx_deriving_biniou https://github.com/lesboloss-es/camelotte.git
```

Note: This library is not yet published to opam-repository. If you are
interested in using it and would like to see it published on OPAM, please let us
know by opening an issue!

### Using in your project

Add `ppx_deriving_biniou` to your `dune` file:

```lisp
(library
 (name mylib)
 (libraries biniou)
 (preprocess (pps ppx_deriving_biniou)))
```

## Usage

### Naming convention

Generated function names are based on the type name:

- For `type t`, functions are named `to_biniou`, `of_biniou_exn`, `of_biniou`
- For `type person`, functions are named `person_to_biniou`, `person_of_biniou_exn`, `person_of_biniou`

### Basic example

Annotate your type definitions with `[@@deriving biniou]`:

```ocaml
type person = {
  name: string;
  age: int;
  email: string option;
} [@@deriving biniou]

(* This generates three functions:
   - val person_to_biniou : person -> Bi_io.tree
   - val person_of_biniou_exn : Bi_io.tree -> person
   - val person_of_biniou : Bi_io.tree -> (person, string * Bi_io.tree) result
*)

let alice = { name = "Alice"; age = 30; email = Some "alice@example.com" }
let tree = person_to_biniou alice
let alice' = person_of_biniou_exn tree
```

When the type is named `t`, the functions have shorter names:

```ocaml
type t = {
  x: int;
  y: int;
} [@@deriving biniou]

(* Generates: to_biniou, of_biniou_exn, of_biniou *)

let point = { x = 10; y = 20 }
let tree = to_biniou point
let point' = of_biniou_exn tree
```

### Separate derivers

You can derive only serialisation or only deserialisation:

```ocaml
type output = string list [@@deriving to_biniou]
(* Generates only: val output_to_biniou : output -> Bi_io.tree *)

type input = int * float [@@deriving of_biniou]
(* Generates only: val input_of_biniou_exn : Bi_io.tree -> input
                   val input_of_biniou : Bi_io.tree -> (input, string * Bi_io.tree) result *)
```

### Supported types

#### Records

Records are serialised as `` `Record ``.

```ocaml
type config = {
  host: string;
  port: int;
  timeout: float;
} [@@deriving biniou]
```

#### Variants

Variants are serialised as `` `Num_variant ``.

```ocaml
type result =
  | Success of int
  | Error of string
  | Pending
[@@deriving biniou]

type message =
  | Text of string
  | Data of { timestamp: int64; payload: string }
[@@deriving biniou]
```

#### Polymorphic variants

Polymorphic variants are serialised as `` `Variant ``. This makes them
compatible with others of the same name.

```ocaml
type status = [
  | `Online
  | `Offline
  | `Away of int
] [@@deriving biniou]

type status2 = [
  | `Online
  | `Offline
] [@@deriving biniou]

(* Polymorphic variants are compatible with one another: *)
let x : status2 = `Online
let tree = status2_to_biniou x
let y : status = status_of_biniou_exn tree
```

#### Tuples

Tuples are serialised as `` `Tuple ``.

```ocaml
type coordinate = float * float [@@deriving biniou]
type triple = int * string * bool [@@deriving biniou]
```

#### Parametric types

```ocaml
type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree
[@@deriving biniou]

(* Generates:
   val tree_to_biniou : ('a -> Bi_io.tree) -> 'a tree -> Bi_io.tree
   val tree_of_biniou_exn : (Bi_io.tree -> 'a) -> Bi_io.tree -> 'a tree
   val tree_of_biniou : (Bi_io.tree -> 'a) -> Bi_io.tree -> ('a tree, string * Bi_io.tree) result
*)

(* Usage: *)
let int_tree_to_biniou = tree_to_biniou Ppx_deriving_biniou_runtime.int_to_biniou
let int_tree_of_biniou_exn = tree_of_biniou_exn Ppx_deriving_biniou_runtime.int_of_biniou_exn
```

#### Recursive types

```ocaml
type expr =
  | Int of int
  | Add of expr * expr
  | Mul of expr * expr
[@@deriving biniou]

let e = Add (Int 2, Mul (Int 3, Int 4))
let tree = expr_to_biniou e
let e' = expr_of_biniou_exn tree
```

#### Built-in types

The following built-in types have automatic support:

| OCaml type  | Biniou representation |
|-------------|-----------------------|
| `unit`      | `` `Unit ``           |
| `bool`      | `` `Bool b ``         |
| `char`      | `` `Int8 c ``         |
| `string`    | `` `String s ``       |
| `int`       | `` `Svint x `` (signed variable-length integer) |
| `int32`     | `` `Int32 x ``        |
| `int64`     | `` `Int64 x ``        |
| `float`     | `` `Float64 x ``      |
| `'a option` | `` `Num_variant (0, None) `` or `` `Num_variant (1, Some tree) `` |
| `('a, 'e) result` | `` `Num_variant (0 \| 1, Some tree) `` |
| `'a array`  | `` `Tuple [\|...\|] ``  |
| `'a list`   | `` `Tuple [\|...\|] ``  |

## API reference

### Generated functions

For each type `t` with `[@@deriving biniou]`, the following functions are
generated:

#### `to_biniou : t -> Bi_io.tree`

Converts a value of type `t` to a Biniou tree.

For types with other names (e.g., `person`), the function is named
`person_to_biniou`.

#### `of_biniou_exn : Bi_io.tree -> t`

Converts a Biniou tree back to a value of type `t`. Raises
[`Ppx_deriving_biniou_runtime.Could_not_convert`] if the conversion fails.

For types with other names (e.g., `person`), the function is named
`person_of_biniou_exn`.

#### `of_biniou : Bi_io.tree -> (t, string * Bi_io.tree) result`

Safe version that returns a `Result` instead of raising an exception. The error
contains the function name where the conversion failed and the tree that
couldn't be converted.

For types with other names (e.g., `person`), the function is named
`person_of_biniou`.

### Composition and custom converters

Only `to_biniou` and `of_biniou_exn` are needed for composition. When writing
custom converters by hand, the proper way to signal conversion failure is to use
[`Ppx_deriving_biniou_runtime.could_not_convert`]:


```ocaml
let my_type_of_biniou_exn : Bi_io.tree -> my_type = function
  | `String "valid" -> My_constructor
  | t -> Ppx_deriving_biniou_runtime.could_not_convert "my_type_of_biniou_exn" t
```

There is a helper, [`Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn`]
doing what its name suggests:

``` ocaml
let my_type_of_biniou_exn : Bi_io.tree -> my_type = ...
let my_type_of_biniou : Bi_io.tree -> (my_type, (string * Bi_io.tree)) result =
  Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn my_type_of_biniou_exn

let my_other_type_of_biniou_exn : (Bi_io.tree -> 'a) -> 'a my_other_type = ...
let my_other_type_of_biniou : (Bi_io.tree -> 'a) -> Bi_io.tree -> ('a my_other_type, (string * Bi_io.tree)) result =
  fun a_of_biniou_exn -> Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn (my_other_type_of_biniou_exn a_of_biniou_exn)
```

### Parametric types

For parametric types, the generated functions take serialisers/deserialisers for
each type parameter:

```ocaml
type ('a, 'b) pair = 'a * 'b [@@deriving biniou]

(* Generates:
   val pair_to_biniou : ('a -> Bi_io.tree) -> ('b -> Bi_io.tree) -> ('a, 'b) pair -> Bi_io.tree
   val pair_of_biniou_exn : (Bi_io.tree -> 'a) -> (Bi_io.tree -> 'b) -> Bi_io.tree -> ('a, 'b) pair
   val pair_of_biniou : (Bi_io.tree -> 'a) -> (Bi_io.tree -> 'b) -> Bi_io.tree -> (('a, 'b) pair, string * Bi_io.tree) result
*)
```

Note that, even though `of_biniou` returns a `Result`, it still expects the
exception-raising variants (`*_of_biniou_exn`) as arguments, not the pure
`Result`-returning variants. For example, use:

```ocaml
let int_tree_of_biniou = tree_of_biniou int_of_biniou_exn
```

and not:

``` ocaml
let int_tree_of_biniou = tree_of_biniou int_of_biniou
```

### Runtime library

The runtime library [`Ppx_deriving_biniou_runtime`] provides converters for
built-in types and helper functions. You typically don't need to use it
directly, but it's available if you need to write custom converters.

## Options

### `alias` option

By default, both `of_biniou_exn` and `of_biniou` are generated. You can disable the non-exception version:

```ocaml
type t = int * string [@@deriving biniou {alias = false}]
(* Only generates: to_biniou and of_biniou_exn *)
```

## Limitations

Many OCaml type features are not currently supported (function types, object
types, first-class modules, etc.). Attempting to derive these will result in a
compile-time error with a descriptive message.

If you need support for a specific type feature, please open an issue. We are
happy to add support on demand!

## Comparison to Atdgen

[Atdgen] is a more comprehensive and mature solution for working with Biniou
(and JSON):

### Atdgen advantages

- **Multi-language support**: Generate code for OCaml, Python, Java, Scala, TypeScript, and more
- **Fine-grained control**: Extensive annotations and customisation options
- **Direct code generation**: Can generate optimised readers/writers directly instead of going through `Bi_io.tree`
- **More efficient**: Better performance for large-scale serialisation
- **Schema validation**: ATD provides a type definition language with validation

### `ppx_deriving_biniou` advantages

- **Simplicity**: Works like other `ppx_deriving` plugins; minimal learning curve
- **Drop-in replacement**: Can replace `ppx_deriving_yojson` with minimal changes
- **No separate files**: Type definitions stay in OCaml files
- **Quick prototyping**: Perfect for rapid development and small projects
- **Standard OCaml workflow**: No need to learn ATD syntax or manage separate `.atd` files

### When to use which?

- **Use `ppx_deriving_biniou`** when:
  - You want quick and easy serialisation for OCaml-only projects
  - You're already familiar with `ppx_deriving` plugins
  - You need simple serialisation without complex requirements
  - You're prototyping or working on small-to-medium projects

- **Use Atdgen** when:
  - You need to share data structures across multiple languages
  - You require fine-grained control over serialisation
  - Performance is critical for your use case
  - You want schema validation and evolution support
  - You're building a large-scale system with formal type specifications

## Example: complete workflow

```ocaml
(* Define your types *)
type user = {
  id: int;
  username: string;
  roles: string list;
} [@@deriving biniou]

type database = {
  users: user list;
  version: int;
} [@@deriving biniou]

(* Create some data *)
let db = {
  users = [
    { id = 1; username = "alice"; roles = ["admin"; "user"] };
    { id = 2; username = "bob"; roles = ["user"] };
  ];
  version = 1;
}

(* Serialise to tree *)
let tree = database_to_biniou db

(* Write to file using Biniou's I/O functions *)
let write_to_file filename tree =
  let oc = open_out_bin filename in
  Bi_io.write oc tree;
  close_out oc

(* Read from file *)
let read_from_file filename =
  let ic = open_in_bin filename in
  let tree = Bi_io.read ic in
  close_in ic;
  tree

(* Full roundtrip *)
let () =
  write_to_file "database.bin" tree;
  let loaded_tree = read_from_file "database.bin" in
  let db' = database_of_biniou_exn loaded_tree in
  assert (db = db')
```

[ppx_deriving]: https://ocaml.org/p/ppx_deriving/
[Biniou]: https://ocaml.org/p/biniou/
[`Bi_io.tree`]: https://ocaml.org/p/biniou/latest/doc/Bi_io/index.html#type-tree
[`Ppx_deriving_biniou_runtime`]: https://lesboloss-es.github.io/Camelotte/ppx_deriving_biniou/Ppx_deriving_biniou_runtime/
[`Ppx_deriving_biniou_runtime.Could_not_convert`]: https://lesboloss-es.github.io/Camelotte/ppx_deriving_biniou/Ppx_deriving_biniou_runtime/#exception-Could_not_convert
[`Ppx_deriving_biniou_runtime.could_not_convert`]: https://lesboloss-es.github.io/Camelotte/ppx_deriving_biniou/Ppx_deriving_biniou_runtime/#val-could_not_convert
[`Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn`]: https://lesboloss-es.github.io/Camelotte/ppx_deriving_biniou/Ppx_deriving_biniou_runtime/#val-of_biniou_of_of_biniou_exn
[Atdgen]: https://atd.readthedocs.io/en/latest/
