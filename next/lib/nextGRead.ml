type 'a cast = string -> 'a

let cast = Fun.id

let int s =
  match int_of_string_opt s with
  | None -> failwith "ExtRead.int"
  | Some n -> n

let bit s = int s = 1

let float s =
  match float_of_string_opt s with
  | None -> failwith "ExtRead.float"
  | Some f -> f

let char x =
  if String.length x <> 1 then
    failwith "ExtRead.char";
  x.[0]

let string s = s

let no_space_string s =
  if String.index_opt s ' ' <> None then
    failwith "ExtRead.no_space_string";
  s

let blank = Str.regexp "[ \t]+"

let list ?(sep = blank) cast input =
  Str.split sep input |> List.map cast

let non_empty_list ?sep cast input =
  let result = list ?sep cast input in
  if result = [] then failwith "ExtRead.non_empty_list";
  result

let array ?sep cast input =
  list ?sep cast input |> Array.of_list

let non_empty_array ?sep cast input =
  non_empty_list ?sep cast input |> Array.of_list

let tuple2 ?(sep = blank) cast1 cast2 input =
  match Str.bounded_split sep input 2 with
  | [value1; value2] -> (cast1 value1, cast2 value2)
  | _ -> failwith "ExtRead.tuple2"

let pair = tuple2

let tuple3 ?(sep = blank) cast1 cast2 cast3 input =
  match Str.bounded_split sep input 3 with
  | [value1; value2; value3] -> (cast1 value1, cast2 value2, cast3 value3)
  | _ -> failwith "ExtRead.tuple3"

let triple = tuple3

let tuple4 ?(sep = blank) cast1 cast2 cast3 cast4 input =
  match Str.bounded_split sep input 4 with
  | [value1; value2; value3; value4] ->
    (cast1 value1, cast2 value2, cast3 value3, cast4 value4)
  | _ -> failwith "ExtRead.tuple4"

let tuple5 ?(sep = blank) cast1 cast2 cast3 cast4 cast5 input =
  match Str.bounded_split sep input 5 with
  | [value1; value2; value3; value4; value5] ->
    (cast1 value1, cast2 value2, cast3 value3, cast4 value4, cast5 value5)
  | _ -> failwith "ExtRead.tuple5"

let tuple6 ?(sep = blank) cast1 cast2 cast3 cast4 cast5 cast6 input =
  match Str.bounded_split sep input 6 with
  | [value1; value2; value3; value4; value5; value6] ->
    (cast1 value1, cast2 value2, cast3 value3, cast4 value4, cast5 value5, cast6 value6)
  | _ -> failwith "ExtRead.tuple6"

let of_string cast s = cast s

let line_of_chan ichan cast = input_line ichan |> string cast
let line cast = line_of_chan stdin cast

let lines_of_chan_until_empty ichan cast =
  let rec aux acc =
    match input_line ichan with
    | exception End_of_file when acc = [] -> raise End_of_file
    | exception End_of_file -> List.rev acc
    | "" -> List.rev acc
    | line -> aux @@ (line |> string cast) :: acc
  in
  aux []
let lines_until_empty cast = lines_of_chan_until_empty stdin cast
