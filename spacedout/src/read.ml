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

let list ?(sep=blank) cast input =
  Str.split sep input |> List.map cast

let non_empty_list ?sep cast input =
  let result = list ?sep cast input in
  if result = [] then failwith "ExtRead.non_empty_list";
  result

let array ?sep cast input =
  list ?sep cast input |> Array.of_list

let non_empty_array ?sep cast input =
  non_empty_list ?sep cast input |> Array.of_list

let tuple2 ?(sep=blank) cast1 cast2 input =
  match Str.bounded_split sep input 2 with
  | [value1; value2] -> (cast1 value1, cast2 value2)
  | _ -> failwith "ExtRead.tuple2"

let pair = tuple2

let tuple3 ?(sep=blank) cast1 cast2 cast3 input =
  match Str.bounded_split sep input 3 with
  | [value1; value2; value3] -> (cast1 value1, cast2 value2, cast3 value3)
  | _ -> failwith "ExtRead.tuple3"

let triple = tuple3

let tuple4 ?(sep=blank) cast1 cast2 cast3 cast4 input =
  match Str.bounded_split sep input 4 with
  | [value1; value2; value3; value4] ->
    (cast1 value1, cast2 value2, cast3 value3, cast4 value4)
  | _ -> failwith "ExtRead.tuple4"

let tuple5 ?(sep=blank) cast1 cast2 cast3 cast4 cast5 input =
  match Str.bounded_split sep input 5 with
  | [value1; value2; value3; value4; value5] ->
    (cast1 value1, cast2 value2, cast3 value3, cast4 value4, cast5 value5)
  | _ -> failwith "ExtRead.tuple5"

let tuple6 ?(sep=blank) cast1 cast2 cast3 cast4 cast5 cast6 input =
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

let%test_module _ = (module struct
  let test cast string expected =
    let result = try Ok (of_string cast string) with exn -> Error exn in
    match result, expected with
    | Ok result, Ok expected when result = expected -> true
    | Error result, Error expected when expected result -> true
    | _ -> false

  (* let any _ = true *)
  let failure = function Failure _ -> true | _ -> false
  (* let invalid_arg = function Invalid_argument _ -> true | _ -> false *)

  let%test _ = test int "7" (Ok 7)
  let%test _ = test int "L" (Error failure)

  let%test _ = test bit "1" (Ok true)
  let%test _ = test bit "0" (Ok false)
  let%test _ = test bit "T" (Error failure)

  let%test _ = test float "34.2" (Ok 34.2)
  let%test _ = test float "TRUE" (Error failure)

  let%test _ = test char "Y" (Ok 'Y')
  let%test _ = test char "YO" (Error failure)

  let%test _ = test string "Bonjour" (Ok "Bonjour")

  let%test _ = test no_space_string "Bonjour" (Ok "Bonjour")
  let%test _ = test no_space_string "Bon jour" (Error failure)

  let%test _ = test (list int) "1 2 7" (Ok [1; 2; 7])
  let%test _ = test (list int) "1 L 7" (Error failure)
  let%test _ = test (list int) "" (Ok [])
  let%test _ = test (non_empty_list int) "" (Error failure)

  let%test _ = test (array int) "1 2 7" (Ok [|1; 2; 7|])
  let%test _ = test (array int) "1 L 7" (Error failure)
  let%test _ = test (array int) "" (Ok [||])
  let%test _ = test (non_empty_array int) "" (Error failure)

  let%test _ = test (pair int int) "7 8" (Ok (7, 8))
  let%test _ = test (pair int int) "7 L" (Error failure)
  let%test _ = test (pair int int) "7" (Error failure)
  let%test _ = test (pair int int) "7 8 9" (Error failure)

  let%test _ = test (pair int float) "7 34.2" (Ok (7, 34.2))
  let%test _ = test (tuple2 int int) "8 9" (Ok (8, 9))
  let%test _ = test (tuple2 int float) "7 34.2" (Ok (7, 34.2))

  let%test _ = test (tuple3 int int int) "7 8 9" (Ok (7, 8, 9))
  let%test _ = test (tuple3 int int int) "7 L 9" (Error failure)
  let%test _ = test (tuple3 int int int) "7 8" (Error failure)
  let%test _ = test (tuple3 int int int) "7 8 9 10" (Error failure)

  let%test _ = test (tuple3 int float string) "7 8 9" (Ok (7, 8., "9"))
  let%test _ = test (tuple3 int float string) "7 8" (Error failure)
  let%test _ = test (tuple3 int float string) "7 8 9 10" (Ok (7, 8., "9 10"))
  let%test _ = test (tuple3 int float no_space_string) "7 8 9 10" (Error failure)

  let%test _ = test (tuple4 int int int int) "7 8 9 10" (Ok (7, 8, 9, 10))
  let%test _ = test (tuple4 int float char string) "7 8 9 10" (Ok (7, 8., '9', "10"))
  let%test _ = test (tuple4 int float char string) "7 8 9 10 11" (Ok (7, 8., '9', "10 11"))
  let%test _ = test (tuple4 int float char no_space_string) "7 8 9 10 11" (Error failure)

  let%test _ = test (tuple5 int int int int int) "7 8 9 10 11" (Ok (7, 8, 9, 10, 11))
  let%test _ = test (tuple5 int float bit char string) "7 8 1 9 10" (Ok (7, 8., true, '9', "10"))
  let%test _ = test (tuple5 int float bit char string) "7 8 0 9 10 11" (Ok (7, 8., false, '9', "10 11"))
  let%test _ = test (tuple5 int float bit char no_space_string) "7 8 0 9 10 11" (Error failure)

  let%test _ = test (pair int (list string)) "7 8 9 10" (Ok (7, ["8"; "9"; "10"]))
  let%test _ = test (pair int (list string)) "7" (Error failure)

  let%test _ = test (pair int (array string)) "7 8 9 10" (Ok (7, [|"8"; "9"; "10"|]))
  let%test _ = test (pair int (array string)) "7" (Error failure)

  let comma = Str.regexp "[ \t]*,[ \t]*"

  let%test _ = test (list ~sep:comma int) "1,2,7" (Ok [1; 2; 7])
  let%test _ = test (list ~sep:comma int) "1, 2 ,7" (Ok [1; 2; 7])
  let%test _ = test (list ~sep:comma int) "1   , 2 , 7" (Ok [1; 2; 7])
  let%test _ = test (list ~sep:comma int) "1  , , 2 , 7" (Error failure)

  let%test _ = test (triple ~sep:comma int int int) "1,2,7" (Ok (1, 2, 7))
  let%test _ = test (triple ~sep:comma int int int) "1, 2 ,7" (Ok (1, 2, 7))
  let%test _ = test (triple ~sep:comma int int int) "1   , 2 , 7" (Ok (1, 2, 7))
  let%test _ = test (triple ~sep:comma int int int) "1  , , 2 , 7" (Error failure)

  let dash = Str.regexp "-"

  let%test _ = test (list ~sep:comma (pair ~sep:dash int float)) "2-4,6-8" (Ok [(2, 4.); (6, 8.)])
  let%test _ = test (list ~sep:comma (pair ~sep:dash int float)) "2-4, 6-8" (Ok [(2, 4.); (6, 8.)])
  let%test _ = test (list ~sep:comma (pair ~sep:dash int float)) "2-4  ,6-8" (Ok [(2, 4.); (6, 8.)])
  let%test _ = test (list ~sep:comma (pair ~sep:dash int float)) "2-4-3  ,6-8" (Error failure)
  let%test _ = test (list ~sep:comma (pair ~sep:dash int float)) "2,4-5  ,6-8" (Error failure)
end)
