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

let unit_to_biniou : unit -> tree = fun () -> `Unit
let unit_of_biniou_exn : tree -> unit = function `Unit -> () | t -> could_not_convert "unit_of_biniou" t

let bool_to_biniou : bool -> tree = fun b -> `Bool b
let bool_of_biniou_exn : tree -> bool = function `Bool b -> b | t -> could_not_convert "bool_of_biniou" t

let char_to_biniou : char -> tree = fun c -> `Int8 c
let char_of_biniou_exn : tree -> char = function `Int8 c -> c | t -> could_not_convert "char_of_biniou" t

let string_to_biniou : string -> tree = fun s -> `String s
let string_of_biniou_exn : tree -> string = function `String s -> s | t -> could_not_convert "string_of_biniou" t

(* NOTE: We use signed variable-length integers to be most flexible, but we will
   need to give the users the choice in the end. *)
let int_to_biniou : int -> tree = fun x -> `Svint x
let int_of_biniou_exn : tree -> int = function
  | `Int8 x -> Char.code x
  | `Int16 x | `Uvint x | `Svint x -> x
  | t -> could_not_convert "int_of_biniou" t

let int32_to_biniou : int32 -> tree = fun x -> `Int32 x
let int32_of_biniou_exn : tree -> int32 = function `Int32 x -> x | t -> could_not_convert "int32_of_biniou" t

let int64_to_biniou : int64 -> tree = fun x -> `Int64 x
let int64_of_biniou_exn : tree -> int64 = function `Int64 x -> x | t -> could_not_convert "int64_of_biniou" t

let float_to_biniou : float -> tree = fun x -> `Float64 x
let float_of_biniou_exn : tree -> float = function `Float32 x | `Float64 x -> x | t -> could_not_convert "float_of_biniou" t

let option_to_biniou (a_to_biniou : 'a -> tree) : 'a option -> tree = function
  | None -> `Num_variant (0, None)
  | Some x -> `Num_variant (1, Some (a_to_biniou x))
let option_of_biniou_exn (a_of_biniou : tree -> 'a) : tree -> 'a option = function
  | `Num_variant (0, None) -> None
  | `Num_variant (1, Some x) -> Some (a_of_biniou x)
  | t -> could_not_convert "option_to_biniou" t

let result_to_biniou (a_to_biniou : 'a -> tree) (e_to_biniou : 'e -> tree) : ('a, 'e) result -> tree = function
  | Ok x -> `Num_variant (0, Some (a_to_biniou x))
  | Error y -> `Num_variant (1, Some (e_to_biniou y))
let result_of_biniou_exn (a_of_biniou : tree -> 'a) (e_of_biniou : tree -> 'e) : tree -> ('a, 'e) result = function
  | `Num_variant (0, Some x) -> Ok (a_of_biniou x)
  | `Num_variant (1, Some y) -> Ok (e_of_biniou y)
  | t -> could_not_convert "result_of_biniou" t

(* NOTE: Biniou has a notion of arrays, but they are really meant as
   heterogeneous arrays, which does not match the notion in OCaml. *)
let array_to_biniou (a_to_biniou : 'a -> tree) : 'a array -> tree = fun l -> `Tuple (Array.map a_to_biniou l)
let array_of_biniou_exn (a_of_biniou : tree -> 'a) : tree -> 'a array = function `Tuple a -> Array.map a_of_biniou a | t -> could_not_convert "array_of_biniou" t

(* NOTE: Biniou has a notion of arrays, but they are really meant as
   heterogeneous arrays, which does not match the notion in OCaml. *)
let list_to_biniou (a_to_biniou : 'a -> tree) : 'a list -> tree = fun l -> `Tuple (Array.of_list (List.map a_to_biniou l))
let list_of_biniou_exn (a_of_biniou : tree -> 'a) : tree -> 'a list = function `Tuple a -> List.map a_of_biniou (Array.to_list a) | t -> could_not_convert "list_of_biniou" t

(** Helper to make a pure [of_biniou] function based on an exception-raising
    [of_biniou_exn] function. *)
let of_biniou_of_of_biniou_exn (of_biniou_exn : tree -> 'a) : tree -> ('a, (string * tree)) result = fun x ->
  try
    Ok (of_biniou_exn x)
  with
    | Could_not_convert (where, what) -> Error (where, what)

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
