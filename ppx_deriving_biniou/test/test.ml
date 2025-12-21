module Basic = struct
  type t = {
    bar: int;
    baz: float;
    qux: int32 list;
    quux: int64 array;
  }
  [@@deriving ord, show {with_path = false}, biniou {alias = false}, qcheck]
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

let roundtrip_count = 10_000

(** Make an Alcotest test case for a type and functions around that type. *)
let roundtrip_test_case ?gen ?show ?arbitrary ~compare ~to_biniou ~of_biniou name =
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
  QCheck_alcotest.to_alcotest @@
  QCheck.Test.make ~name ~count: roundtrip_count arbitrary @@ fun x ->
  let xx = of_biniou (to_biniou x) in
  if compare x xx <> 0 then
    QCheck.Test.fail_reportf "Failed during simple roundtrip (without anonymisation).@\nAfter roundtrip: %s" (show xx);
  let xx = of_biniou (anonymise_tree (to_biniou x)) in
  if compare x xx <> 0 then
    QCheck.Test.fail_reportf "Failed during roundtrip with anonymisation.@\nAfter roundtrip: %s" (show xx);
  true

module type Roundtrip_test_caseable = sig
  type t [@@deriving ord, show]
  val to_biniou : t -> Bi_io.tree
  val of_biniou_exn : Bi_io.tree -> t
  val gen : t QCheck.Gen.t
end

let roundtrip_test_case' name (module R : Roundtrip_test_caseable) =
  roundtrip_test_case name ~gen: R.gen ~show: R.show ~compare: R.compare ~to_biniou: R.to_biniou ~of_biniou: R.of_biniou_exn

let () =
  Alcotest.run "ppx_deriving_biniou" [
    (
      "roundrip",
      [roundtrip_test_case "int" ~arbitrary: QCheck.int ~compare: Int.compare ~to_biniou: Ppx_deriving_biniou_runtime.int_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int_of_biniou_exn;
      roundtrip_test_case "int32" ~arbitrary: QCheck.int32 ~compare: Int32.compare ~to_biniou: Ppx_deriving_biniou_runtime.int32_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int32_of_biniou_exn;
      roundtrip_test_case "int64" ~arbitrary: QCheck.int64 ~compare: Int64.compare ~to_biniou: Ppx_deriving_biniou_runtime.int64_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.int64_of_biniou_exn;
      roundtrip_test_case "float" ~arbitrary: QCheck.float ~compare: Float.compare ~to_biniou: Ppx_deriving_biniou_runtime.float_to_biniou ~of_biniou: Ppx_deriving_biniou_runtime.float_of_biniou_exn;
      roundtrip_test_case "int32 list" ~arbitrary: QCheck.(list int32) ~compare: (List.compare Int32.compare) ~to_biniou: Ppx_deriving_biniou_runtime.(list_to_biniou int32_to_biniou) ~of_biniou: Ppx_deriving_biniou_runtime.(list_of_biniou_exn int32_of_biniou_exn);
      (* roundtrip_test_case "int64 array" ~arbitrary: QCheck.(array int64) ~compare: (Array.compare Int64.compare) ~to_biniou: Ppx_deriving_biniou_runtime.(array_to_biniou int64_to_biniou) ~of_biniou: Ppx_deriving_biniou_runtime.(array_of_biniou int64_of_biniou); *)
      roundtrip_test_case' "basic" (module Basic);
      ]
    )
  ]
