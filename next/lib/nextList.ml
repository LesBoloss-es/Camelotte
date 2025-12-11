include List

let union_sorted compare l1 l2 =
  let rec union_sorted acc l1 l2 =
    match l1, l2 with
    | [], [] -> rev acc
    | _, [] -> rev_append acc l1
    | [], _ -> rev_append acc l2
    | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c < 0 then
        union_sorted (h1 :: acc) t1 l2
      else if c > 0 then
        union_sorted (h2 :: acc) l1 t2
      else
        union_sorted (h1 :: acc) t1 t2
  in
  union_sorted [] l1 l2

let inter_sorted compare l1 l2 =
  let rec inter_sorted acc l1 l2 =
    match l1, l2 with
    | _, [] | [], _ -> rev acc
    | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c < 0 then
        inter_sorted acc t1 l2
      else if c > 0 then
        inter_sorted acc l1 t2
      else
        inter_sorted (h1 :: acc) t1 t2
  in
  inter_sorted [] l1 l2

let diff_sorted compare l1 l2 =
  let rec diff_sorted acc l1 l2 =
    match l1, l2 with
    | [], _ -> rev acc
    | l1, [] -> rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c < 0 then
        diff_sorted (h1 :: acc) t1 l2
      else if c > 0 then
        diff_sorted acc l1 t2
      else
        diff_sorted acc t1 l2
  in
  diff_sorted [] l1 l2

let symdiff_sorted compare l1 l2 =
  let rec symdiff_sorted acc l1 l2 =
    match l1, l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
      let c = compare h1 h2 in
      if c < 0 then
        symdiff_sorted (h1 :: acc) t1 l2
      else if c > 0 then
        symdiff_sorted (h2 :: acc) l1 t2
      else
        symdiff_sorted acc t1 t2
  in
  symdiff_sorted [] l1 l2

let rec hdn n l =
  if n = 0 then []
  else
    match l with
    | [] -> []
    | h :: q -> h :: hdn (n - 1) q

let rec bd = function
  | [] -> failwith "ExtList.bd"
  | [_] -> []
  | x :: l -> x :: bd l

let rec ft = function
  | [] -> failwith "ExtList.ft"
  | [e] -> e
  | _ :: l -> ft l

let init_until f =
  let rec aux i acc =
    match f i with
    | None -> acc
    | Some x -> aux (i + 1) (x :: acc)
  in
  rev @@ aux 0 []

let sub l pos len =
  if pos < 0 || len < 0 then
    invalid_arg "ExtList.sub";
  let rec go_to_pos i = function
    | xs when i = pos -> gather_len 0 [] xs
    | [] -> invalid_arg "ExtList.sub"
    | _ :: xs -> go_to_pos (i + 1) xs
  and gather_len j acc = function
    | _ when j = len -> List.rev acc
    | [] -> invalid_arg "ExtList.sub"
    | x :: xs -> gather_len (j + 1) (x :: acc) xs
  in
  go_to_pos 0 l

let take n l = sub l 0 n
let drop n l = sub l n (length l - n)

let count p l =
  let rec count i = function
    | [] -> i
    | x :: l -> count (if p x then i + 1 else i) l
  in
  count 0 l

let singleton x = [x]

let rec update_nth n f = function
  | [] -> failwith "ExtRead.update_nth"
  | x :: xs when n = 0 -> f x :: xs
  | x :: xs -> x :: update_nth (n - 1) f xs

let rec prefix_until_inclusive p = function
  | [] -> []
  | x :: _ when p x -> [x]
  | x :: xs -> x :: prefix_until_inclusive p xs
