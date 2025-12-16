type node_tag = int [@@deriving show]
type hash = int [@@deriving show]
type tree = [%import: Bi_io.tree] [@@deriving show]

exception Could_not_convert of string * tree
let could_not_convert name tree = raise (Could_not_convert (name, tree))

let () =
  Printexc.register_printer @@ function
    | Could_not_convert (name, tree) ->
      Some (Format.asprintf "Could_not_convert \"%s\" %a" name pp_tree tree)
    | _ -> None

(* NOTE: We use signed variable-length integers to be most flexible, but we will
   need to give the users the choice in the end. *)
let int_to_biniou : int -> tree = fun x -> `Svint x
let int_of_biniou : tree -> int = function
  | `Int8 x -> Char.code x
  | `Int16 x | `Uvint x | `Svint x -> x
  | t -> could_not_convert "int_of_biniou" t

let float_to_biniou : float -> tree = fun x -> `Float64 x
let float_of_biniou : tree -> float = function
  | `Float32 x | `Float64 x -> x
  | t -> could_not_convert "float_of_biniou" t

(** Given the content of a Biniou record, look for the tree corresponding to the
    given string by comparing it to the hashes. If it isn't found, the name
    argument is used to raise {!Could_not_convert}. *)
let record_find : name: string -> string -> (string option * hash * tree) array -> tree = fun ~name string record ->
  let hash = Bi_io.hash_name string in
  let tree_option =
    Array.find_map
      (fun (_, hash', tree) -> if hash = hash' then Some tree else None)
      record
  in
  match tree_option with
  | None -> could_not_convert name (`Record record)
  | Some tree -> tree
