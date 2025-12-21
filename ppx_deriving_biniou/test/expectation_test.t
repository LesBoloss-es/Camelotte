Test of generated code for ppx_deriving_biniou
==============================================

Tests of the user interface of the PPX, in its good and bad cases. This will
only test that the produced code compiles, and allow us to inspect manually that
it has the expected shape. This will not test that the produced code is a good
serialisation. For this, see roundtrip_test.

Setup
-----

Test function that gets some OCaml code on its standard input and run it through
the OCaml compiler, with ppx_deriving_biniou enabled, and outputs the parsetree
as well as the output of the compilation. The output is post-processed to make
it more readable and more deterministic.

  $ test_ppx_deriving_biniou () {
  >   local tmpfile output result
  >   tmpfile=$(mktemp --suffix=.ml)
  >   cat > $tmpfile
  >   output=$(
  >     ocamlfind ocamlc \
  >       -package biniou \
  >       -package ppx_deriving_biniou.runtime \
  >       -ppx 'ppx_deriving_biniou --as-ppx' \
  >       -dsource -c "$tmpfile" \
  >       2>&1
  >   )
  >   result=$?
  >   echo "$output" | sed 's|File "[^"]*", line|Line|'
  >   return $result
  > }

Alias
-----

  $ test_ppx_deriving_biniou <<EOF
  >   type t = string [@@deriving biniou]
  > EOF
  type t = string[@@deriving biniou]
  let rec to_biniou : t -> Bi_io.tree =
    Ppx_deriving_biniou_runtime.string_to_biniou[@@ocaml.warning "-39"]
  let rec of_biniou_exn : Bi_io.tree -> t =
    Ppx_deriving_biniou_runtime.string_of_biniou_exn[@@ocaml.warning "-39"]
  let of_biniou : Bi_io.tree -> (t, (string * Bi_io.tree)) Stdlib.Result.t =
    Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn of_biniou_exn
    [@@ocaml.warning "-32"]

Alias (to_biniou)
-----------------

  $ test_ppx_deriving_biniou <<EOF
  >   type t = string [@@deriving to_biniou]
  >   let _ = of_biniou
  > EOF
  type t = string[@@deriving to_biniou]
  let rec to_biniou : t -> Bi_io.tree =
    Ppx_deriving_biniou_runtime.string_to_biniou[@@ocaml.warning "-39"]
  let _ = of_biniou
  Line 2, characters 10-19:
  2 |   let _ = of_biniou
                ^^^^^^^^^
  Error: Unbound value of_biniou
  Hint: Did you mean to_biniou?
  [2]

Alias (of_biniou)
-----------------

  $ test_ppx_deriving_biniou <<EOF
  >   type t = string [@@deriving of_biniou]
  >   let _ = to_biniou
  > EOF
  type t = string[@@deriving of_biniou]
  let rec of_biniou_exn : Bi_io.tree -> t =
    Ppx_deriving_biniou_runtime.string_of_biniou_exn[@@ocaml.warning "-39"]
  let of_biniou : Bi_io.tree -> (t, (string * Bi_io.tree)) Stdlib.Result.t =
    Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn of_biniou_exn
    [@@ocaml.warning "-32"]
  let _ = to_biniou
  Line 2, characters 10-19:
  2 |   let _ = to_biniou
                ^^^^^^^^^
  Error: Unbound value to_biniou
  Hint: Did you mean of_biniou?
  [2]

Alias with argument
-------------------

  $ test_ppx_deriving_biniou <<EOF
  >   type 'a t = 'a list [@@deriving biniou]
  > EOF
  type 'a t = 'a list[@@deriving biniou]
  let rec to_biniou : ('a -> Bi_io.tree) -> 'a t -> Bi_io.tree =
    fun _tvar_a_to_biniou ->
      Ppx_deriving_biniou_runtime.list_to_biniou _tvar_a_to_biniou[@@ocaml.warning
                                                                    "-39"]
  let rec of_biniou_exn : (Bi_io.tree -> 'a) -> Bi_io.tree -> 'a t =
    fun _tvar_a_of_biniou_exn ->
      Ppx_deriving_biniou_runtime.list_of_biniou_exn _tvar_a_of_biniou_exn
    [@@ocaml.warning "-39"]
  let of_biniou :
    (Bi_io.tree -> 'a) ->
      Bi_io.tree -> ('a t, (string * Bi_io.tree)) Stdlib.Result.t
    =
    fun _tvar_a_of_biniou_exn ->
      Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn
        (of_biniou_exn _tvar_a_of_biniou_exn)[@@ocaml.warning "-32"]

Record
------

  $ test_ppx_deriving_biniou <<EOF
  >   type t = {
  >     foo : int;
  >     bar : float list;
  >   }
  >   [@@deriving biniou]
  > EOF
  type t = {
    foo: int ;
    bar: float list }[@@deriving biniou]
  let rec to_biniou : t -> Bi_io.tree =
    fun r ->
      `Record
        [|((Some "foo"), (Bi_io.hash_name "foo"),
            (Ppx_deriving_biniou_runtime.int_to_biniou r.foo));((Some "bar"),
                                                                 (Bi_io.hash_name
                                                                    "bar"),
                                                                 (Ppx_deriving_biniou_runtime.list_to_biniou
                                                                    Ppx_deriving_biniou_runtime.float_to_biniou
                                                                    r.bar))|]
    [@@ocaml.warning "-39"]
  let rec of_biniou_exn : Bi_io.tree -> t =
    function
    | `Record r ->
        {
          foo =
            (Ppx_deriving_biniou_runtime.int_of_biniou_exn
               (Ppx_deriving_biniou_runtime.record_find ~name:"of_biniou_exn"
                  "foo" r));
          bar =
            (Ppx_deriving_biniou_runtime.list_of_biniou_exn
               Ppx_deriving_biniou_runtime.float_of_biniou_exn
               (Ppx_deriving_biniou_runtime.record_find ~name:"of_biniou_exn"
                  "bar" r))
        }
    | t -> Ppx_deriving_biniou_runtime.could_not_convert "of_biniou_exn" t
    [@@ocaml.warning "-39"]
  let of_biniou : Bi_io.tree -> (t, (string * Bi_io.tree)) Stdlib.Result.t =
    Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn of_biniou_exn
    [@@ocaml.warning "-32"]

Variant
-------

  $ test_ppx_deriving_biniou <<EOF
  >   type t = Foo of int * float [@@deriving biniou]
  > EOF
  type t =
    | Foo of int * float [@@deriving biniou]
  let rec to_biniou : t -> Bi_io.tree =
    function
    | Foo (arg0, arg1) ->
        `Num_variant
          (0,
            (Some
               (`Tuple
                  [|(Ppx_deriving_biniou_runtime.int_to_biniou arg0);(Ppx_deriving_biniou_runtime.float_to_biniou
                                                                      arg1)|])))
    [@@ocaml.warning "-39"]
  let rec of_biniou_exn : Bi_io.tree -> t =
    function
    | `Num_variant (0, Some (`Tuple [|arg0;arg1|])) ->
        Foo
          ((Ppx_deriving_biniou_runtime.int_of_biniou_exn arg0),
            (Ppx_deriving_biniou_runtime.float_of_biniou_exn arg1))
    | t -> Ppx_deriving_biniou_runtime.could_not_convert "of_biniou_exn" t
    [@@ocaml.warning "-39"]
  let of_biniou : Bi_io.tree -> (t, (string * Bi_io.tree)) Stdlib.Result.t =
    Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn of_biniou_exn
    [@@ocaml.warning "-32"]

Variant with record
-------------------

  $ test_ppx_deriving_biniou <<EOF
  >   type t = Foo of {bar: int; baz: float} [@@deriving biniou]
  > EOF
  type t =
    | Foo of {
    bar: int ;
    baz: float } [@@deriving biniou]
  let rec to_biniou : t -> Bi_io.tree =
    function
    | Foo r ->
        `Num_variant
          (0,
            (Some
               (`Record
                  [|((Some "bar"), (Bi_io.hash_name "bar"),
                      (Ppx_deriving_biniou_runtime.int_to_biniou r.bar));
                    ((Some "baz"), (Bi_io.hash_name "baz"),
                      (Ppx_deriving_biniou_runtime.float_to_biniou r.baz))|])))
    [@@ocaml.warning "-39"]
  let rec of_biniou_exn : Bi_io.tree -> t =
    function
    | `Num_variant (0, Some (`Record r)) ->
        Foo
          {
            bar =
              (Ppx_deriving_biniou_runtime.int_of_biniou_exn
                 (Ppx_deriving_biniou_runtime.record_find ~name:"of_biniou_exn"
                    "bar" r));
            baz =
              (Ppx_deriving_biniou_runtime.float_of_biniou_exn
                 (Ppx_deriving_biniou_runtime.record_find ~name:"of_biniou_exn"
                    "baz" r))
          }
    | t -> Ppx_deriving_biniou_runtime.could_not_convert "of_biniou_exn" t
    [@@ocaml.warning "-39"]
  let of_biniou : Bi_io.tree -> (t, (string * Bi_io.tree)) Stdlib.Result.t =
    Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn of_biniou_exn
    [@@ocaml.warning "-32"]

Recursive
---------

  $ test_ppx_deriving_biniou <<EOF
  >   type 'a t = Nil | Cons of 'a * 'a t [@@deriving biniou]
  > EOF
  type 'a t =
    | Nil 
    | Cons of 'a * 'a t [@@deriving biniou]
  let rec to_biniou : ('a -> Bi_io.tree) -> 'a t -> Bi_io.tree =
    fun _tvar_a_to_biniou ->
      (function
       | Nil -> `Num_variant (0, None)
       | Cons (arg0, arg1) ->
           `Num_variant
             (1,
               (Some
                  (`Tuple
                     [|(_tvar_a_to_biniou arg0);((to_biniou _tvar_a_to_biniou)
                                                   arg1)|]))))[@@ocaml.warning
                                                                "-39"]
  let rec of_biniou_exn : (Bi_io.tree -> 'a) -> Bi_io.tree -> 'a t =
    fun _tvar_a_of_biniou_exn ->
      (function
       | `Num_variant (0, None) -> Nil
       | `Num_variant (1, Some (`Tuple [|arg0;arg1|])) ->
           Cons
             ((_tvar_a_of_biniou_exn arg0),
               ((of_biniou_exn _tvar_a_of_biniou_exn) arg1))
       | t -> Ppx_deriving_biniou_runtime.could_not_convert "of_biniou_exn" t)
    [@@ocaml.warning "-39"]
  let of_biniou :
    (Bi_io.tree -> 'a) ->
      Bi_io.tree -> ('a t, (string * Bi_io.tree)) Stdlib.Result.t
    =
    fun _tvar_a_of_biniou_exn ->
      Ppx_deriving_biniou_runtime.of_biniou_of_of_biniou_exn
        (of_biniou_exn _tvar_a_of_biniou_exn)[@@ocaml.warning "-32"]
