include String

let split_on_char_n n char s =
  let rec split_on_char_n acc n ~from_ =
    if n <= 0 then List.rev (sub s from_ (length s - from_) :: acc)
    else
      match index_from_opt s from_ char with
      | None -> failwith "ExtString.split_on_char_n"
      | Some i -> split_on_char_n (sub s from_ (i - from_) :: acc) (n - 1) ~from_: (i + 1)
  in
  if n <= 0 then invalid_arg "ExtString.split_on_char_n"
  else split_on_char_n [] (n - 1) ~from_: 0
