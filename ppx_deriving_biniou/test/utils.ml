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

let roundtrip_count = 100000

let roundtrip_test_case ~name ~arbitrary ~to_biniou ~of_biniou =
  QCheck_alcotest.to_alcotest @@
    QCheck.Test.make ~name ~count: roundtrip_count arbitrary (fun x ->
      of_biniou (to_biniou x) = x && of_biniou (anonymise_tree (to_biniou x)) = x
    )
