module Basic = struct
  type t = {
    un: unit;
    bar: int;
    baar: bool;
    baz: float;
    baaz: string option;
    qux: int32 list;
    quux: int64 array;
  }
  [@@deriving ord, show {with_path = false}, biniou, qcheck]
end

module Alias = struct
  type t = (string * float) option [@@deriving ord, show, biniou, qcheck]
end

module Variant = struct
  type t =
    | Foo of int
    | Bar of string list
    | Baz of float * int32 array
    | Qux of {bar: int64 list; baz: (string * char) array}
  [@@deriving ord, show, biniou, qcheck]
end

module Variant_2 = struct
  type t = [
    | `Foo of int
    | `Bar of string list
    | `Baz of float * int32 array
  ]
  [@@deriving ord, show, biniou, qcheck]
end

module Type_arguments = struct
  (* FIXME: I would like to check that there are no clashes it [type ('a, 'b) s
     = ('a * a * 'b)], by renaming [type c] to [type a], but ppx_deriving_qcheck
     does not handle this situation cleanly. See
     https://github.com/c-cube/qcheck/issues/399 *)
  type c = int [@@deriving ord, show, biniou, qcheck]
  type ('a, 'b) s = 'a * c * 'b [@@deriving ord, show, biniou, qcheck]
  type t = (float, string) s [@@deriving ord, show, biniou, qcheck]
end

(** Biniou trees can contain optional information. This function erases it. It
    is useful in testing context to verify that we still manage to unserialise
    what needs to be unserialised. *)
let rec anonymise_tree : Bi_io.tree -> Bi_io.tree = function
  | `Unit -> `Unit
  | `Bool bool -> `Bool bool
  | `Int8 int -> `Int8 int
  | `Int16 int -> `Int16 int
  | `Int32 int32 -> `Int32 int32
  | `Int64 int64 -> `Int64 int64
  | `Float32 float -> `Float32 float
  | `Float64 float -> `Float64 float
  | `Uvint int -> `Uvint int
  | `Svint int -> `Svint int
  | `String string -> `String string
  | `Array array -> `Array (Option.map (fun (node_tag, tree_array) -> (node_tag, Array.map anonymise_tree tree_array)) array)
  | `Tuple tuple -> `Tuple (Array.map anonymise_tree tuple)
  | `Record record -> `Record (Array.map (fun (_, hash, tree_option) -> (None, hash, tree_option)) record)
  | `Num_variant (num, tree_option) -> `Num_variant (num, Option.map anonymise_tree tree_option)
  | `Variant (_, hash, tree) -> `Variant (None, hash, tree)
  | `Table table ->
    `Table (
      Option.map
        (fun (string_option_hash_node_tag_array, tree_array_array) ->
          Array.map (fun (_, hash, node_tag) -> (None, hash, node_tag)) string_option_hash_node_tag_array,
          Array.map (Array.map anonymise_tree) tree_array_array
        )
        table
    )
  | `Shared tree -> `Shared (anonymise_tree tree)

let roundtrip_count = 1_000

(** Make Alcotest test cases for a type and functions around that type. One of
    the test is simply [of_biniou % to_biniou] while the other goes all the way
    to the string representation. *)
let roundtrip_test_cases ?gen ?show ?arbitrary ~compare ~to_biniou ~of_biniou name =
  (* NOTE: We use [compare] instead of [equal] because [@@deriving eq] uses
     [(=)] on simple types, and for floats this leads to issues with [nan].
     See https://github.com/ocaml-ppx/ppx_deriving/issues/298 *)
  let (arbitrary, show) =
    match arbitrary, gen with
    | Some arbitrary, None -> (arbitrary, QCheck.get_print arbitrary)
    | None, Some gen -> (QCheck.make ?print: show gen, show)
    | _ -> invalid_arg "roundtrip_test_case: no or both arbitrary and gen"
  in
  let show =
    match show with
    | Some show -> show
    | None -> invalid_arg "roundtrip_test_case: no show"
  in
  List.map
    (fun (suffix, roundtrip) ->
      QCheck_alcotest.to_alcotest @@
      QCheck.Test.make ~name: (name ^ suffix) ~count: roundtrip_count arbitrary @@ fun x ->
      let xx = roundtrip x in
      if compare x xx <> 0 then
        QCheck.Test.fail_reportf "Failed.@\nAfter roundtrip: %s" (show xx);
      true
    )
    [
      ("", (fun x -> of_biniou (to_biniou x)));
      (" (anonymised)", (fun x -> of_biniou (anonymise_tree (to_biniou x))));
      (" (full)", (fun x -> of_biniou (Bi_io.tree_of_string (Bi_io.string_of_tree (to_biniou x)))));
    ]

module type Roundtrip_test_caseable = sig
  type t [@@deriving ord, show, biniou {alias = false}]
  val gen : t QCheck.Gen.t
end

let roundtrip_test_cases' name (module R : Roundtrip_test_caseable) =
  roundtrip_test_cases name ~gen: R.gen ~show: R.show ~compare: R.compare ~to_biniou: R.to_biniou ~of_biniou: R.of_biniou_exn

let () =
  Alcotest.run "ppx_deriving_biniou" [
    (
      "roundrip",
      List.flatten
        [
          roundtrip_test_cases "int" ~arbitrary: QCheck.int ~compare: Int.compare ~to_biniou: Ppx_deriving_biniou_runtime.int_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int_of_biniou_exn;
          roundtrip_test_cases "int32" ~arbitrary: QCheck.int32 ~compare: Int32.compare ~to_biniou: Ppx_deriving_biniou_runtime.int32_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int32_of_biniou_exn;
          roundtrip_test_cases "int64" ~arbitrary: QCheck.int64 ~compare: Int64.compare ~to_biniou: Ppx_deriving_biniou_runtime.int64_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int64_of_biniou_exn;
          roundtrip_test_cases "float" ~arbitrary: QCheck.float ~compare: Float.compare ~to_biniou: Ppx_deriving_biniou_runtime.float_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.float_of_biniou_exn;
          roundtrip_test_cases "int32 list" ~arbitrary: QCheck.(list int32) ~compare: (List.compare Int32.compare) ~to_biniou: Ppx_deriving_biniou_runtime.(list_to_biniou int32_to_biniou) ~of_biniou: Ppx_deriving_biniou_runtime.(list_of_biniou_exn int32_of_biniou_exn);
          (* roundtrip_test_cases "int64 array" ~arbitrary: QCheck.(array int64) ~compare: (Array.compare Int64.compare) ~to_biniou: Ppx_deriving_biniou_runtime.(array_to_biniou int64_to_biniou) ~of_biniou: Ppx_deriving_biniou_runtime.(array_of_biniou int64_of_biniou); *)
          roundtrip_test_cases' "basic" (module Basic);
          roundtrip_test_cases' "alias" (module Alias);
          roundtrip_test_cases' "variant" (module Variant);
          roundtrip_test_cases' "variant 2" (module Variant_2);
          roundtrip_test_cases' "type arguments" (module Type_arguments);
        ]
    )
  ]
