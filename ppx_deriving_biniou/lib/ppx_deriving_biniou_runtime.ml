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

let int32_to_biniou : int32 -> tree = fun x -> `Int32 x
let int32_of_biniou : tree -> int32 = function `Int32 x -> x | t -> could_not_convert "int32_of_biniou" t

let int64_to_biniou : int64 -> tree = fun x -> `Int64 x
let int64_of_biniou : tree -> int64 = function `Int64 x -> x | t -> could_not_convert "int64_of_biniou" t

let float_to_biniou : float -> tree = fun x -> `Float64 x
let float_of_biniou : tree -> float = function `Float32 x | `Float64 x -> x | t -> could_not_convert "float_of_biniou" t

(* NOTE: Biniou has a notion of arrays, but they are really meant as
   heterogeneous arrays, which does not match the notion in OCaml. *)
let array_to_biniou (a_to_biniou : 'a -> tree) : 'a array -> tree = fun l -> `Tuple (Array.map a_to_biniou l)
let array_of_biniou (a_of_biniou : tree -> 'a) : tree -> 'a array = function `Tuple a -> Array.map a_of_biniou a | t -> could_not_convert "array_of_biniou" t

(* NOTE: Biniou has a notion of arrays, but they are really meant as
   heterogeneous arrays, which does not match the notion in OCaml. *)
let list_to_biniou (a_to_biniou : 'a -> tree) : 'a list -> tree = fun l -> `Tuple (Array.of_list (List.map a_to_biniou l))
let list_of_biniou (a_of_biniou : tree -> 'a) : tree -> 'a list = function `Tuple a -> List.map a_of_biniou (Array.to_list a) | t -> could_not_convert "list_of_biniou" t

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
