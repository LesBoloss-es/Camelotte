type 'a cast = string -> 'a

(* Readers *)

let string_as cast s = cast s

let line_as cast ichan = string_as cast (input_line ichan)

let lines_until_empty_as cast ichan =
  let rec aux acc =
    match input_line ichan with
    | exception End_of_file when acc = [] -> raise End_of_file
    | exception End_of_file -> List.rev acc
    | "" -> List.rev acc
    | line -> aux (string_as cast line :: acc)
  in
  aux []

(* Casts *)

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

let seq ?sep cast input =
  list ?sep cast input |> List.to_seq

let non_empty_seq ?sep cast input =
  non_empty_list ?sep cast input |> List.to_seq

let array ?sep cast input =
  list ?sep cast input |> Array.of_list

let non_empty_array ?sep cast input =
  non_empty_list ?sep cast input |> Array.of_list

let tuple_2 ?(sep = blank) c1 c2 input =
  match Str.bounded_split sep input 2 with
  | [v1; v2] -> (c1 v1, c2 v2)
  | _ -> failwith "SpacedOut.Read.tuple_2"

let pair = tuple_2
let couple = tuple_2

let tuple_3 ?(sep = blank) c1 c2 c3 input =
  match Str.bounded_split sep input 3 with
  | [v1; v2; v3] -> (c1 v1, c2 v2, c3 v3)
  | _ -> failwith "SpacedOut.Read.tuple_3"

let triple = tuple_3

let tuple_4 ?(sep = blank) c1 c2 c3 c4 input =
  match Str.bounded_split sep input 4 with
  | [v1; v2; v3; v4] -> (c1 v1, c2 v2, c3 v3, c4 v4)
  | _ -> failwith "SpacedOut.Read.tuple_4"

let quadruple = tuple_4

let tuple_5 ?(sep = blank) c1 c2 c3 c4 c5 input =
  match Str.bounded_split sep input 5 with
  | [v1; v2; v3; v4; v5] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5)
  | _ -> failwith "SpacedOut.Read.tuple_5"

let quintuple = tuple_5
let pentuple = tuple_5

let tuple_6 ?(sep = blank) c1 c2 c3 c4 c5 c6 input =
  match Str.bounded_split sep input 6 with
  | [v1; v2; v3; v4; v5; v6] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6)
  | _ -> failwith "SpacedOut.Read.tuple_6"

let sextuple = tuple_6
let hextuple = tuple_6

let tuple_7 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 input =
  match Str.bounded_split sep input 7 with
  | [v1; v2; v3; v4; v5; v6; v7] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7)
  | _ -> failwith "SpacedOut.Read.tuple_7"

let septuple = tuple_7
let heptuple = tuple_7

let tuple_8 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 input =
  match Str.bounded_split sep input 8 with
  | [v1; v2; v3; v4; v5; v6; v7; v8] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7, c8 v8)
  | _ -> failwith "SpacedOut.Read.tuple_8"

let octuple = tuple_8

let tuple_9 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 c9 input =
  match Str.bounded_split sep input 9 with
  | [v1; v2; v3; v4; v5; v6; v7; v8; v9] -> (c1 v1, c2 v2, c3 v3, c4 v4, c5 v5, c6 v6, c7 v7, c8 v8, c9 v9)
  | _ -> failwith "SpacedOut.Read.tuple_9"

let nonuple = tuple_9

(* Tests *)

let%test_module _ =
  (module struct
    let test cast string expected =
      let result = try Ok (string_as cast string) with exn -> Error exn in
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
    let%test _ = test (tuple_2 int int) "8 9" (Ok (8, 9))
    let%test _ = test (tuple_2 int float) "7 34.2" (Ok (7, 34.2))

    let%test _ = test (tuple_3 int int int) "7 8 9" (Ok (7, 8, 9))
    let%test _ = test (tuple_3 int int int) "7 L 9" (Error failure)
    let%test _ = test (tuple_3 int int int) "7 8" (Error failure)
    let%test _ = test (tuple_3 int int int) "7 8 9 10" (Error failure)

    let%test _ = test (tuple_3 int float string) "7 8 9" (Ok (7, 8., "9"))
    let%test _ = test (tuple_3 int float string) "7 8" (Error failure)
    let%test _ = test (tuple_3 int float string) "7 8 9 10" (Ok (7, 8., "9 10"))
    let%test _ = test (tuple_3 int float no_space_string) "7 8 9 10" (Error failure)

    let%test _ = test (tuple_4 int int int int) "7 8 9 10" (Ok (7, 8, 9, 10))
    let%test _ = test (tuple_4 int float char string) "7 8 9 10" (Ok (7, 8., '9', "10"))
    let%test _ = test (tuple_4 int float char string) "7 8 9 10 11" (Ok (7, 8., '9', "10 11"))
    let%test _ = test (tuple_4 int float char no_space_string) "7 8 9 10 11" (Error failure)

    let%test _ = test (tuple_5 int int int int int) "7 8 9 10 11" (Ok (7, 8, 9, 10, 11))
    let%test _ = test (tuple_5 int float bit char string) "7 8 1 9 10" (Ok (7, 8., true, '9', "10"))
    let%test _ = test (tuple_5 int float bit char string) "7 8 0 9 10 11" (Ok (7, 8., false, '9', "10 11"))
    let%test _ = test (tuple_5 int float bit char no_space_string) "7 8 0 9 10 11" (Error failure)

    let%test _ = test (pair int (list string)) "7 8 9 10" (Ok (7, ["8"; "9"; "10"]))
    let%test _ = test (pair int (list string)) "7" (Error failure)

    let%test _ = test (pair int (array string)) "7 8 9 10" (Ok (7, [|"8"; "9"; "10"|]))
    let%test _ = test (pair int (array string)) "7" (Error failure)

    let comma = Str.regexp "[ \t]*,[ \t]*"

    let%test _ = test (list ~sep: comma int) "1,2,7" (Ok [1; 2; 7])
    let%test _ = test (list ~sep: comma int) "1, 2 ,7" (Ok [1; 2; 7])
    let%test _ = test (list ~sep: comma int) "1   , 2 , 7" (Ok [1; 2; 7])
    let%test _ = test (list ~sep: comma int) "1  , , 2 , 7" (Error failure)

    let%test _ = test (triple ~sep: comma int int int) "1,2,7" (Ok (1, 2, 7))
    let%test _ = test (triple ~sep: comma int int int) "1, 2 ,7" (Ok (1, 2, 7))
    let%test _ = test (triple ~sep: comma int int int) "1   , 2 , 7" (Ok (1, 2, 7))
    let%test _ = test (triple ~sep: comma int int int) "1  , , 2 , 7" (Error failure)

    let dash = Str.regexp "-"

    let%test _ = test (list ~sep: comma (pair ~sep: dash int float)) "2-4,6-8" (Ok [(2, 4.); (6, 8.)])
    let%test _ = test (list ~sep: comma (pair ~sep: dash int float)) "2-4, 6-8" (Ok [(2, 4.); (6, 8.)])
    let%test _ = test (list ~sep: comma (pair ~sep: dash int float)) "2-4  ,6-8" (Ok [(2, 4.); (6, 8.)])
    let%test _ = test (list ~sep: comma (pair ~sep: dash int float)) "2-4-3  ,6-8" (Error failure)
    let%test _ = test (list ~sep: comma (pair ~sep: dash int float)) "2,4-5  ,6-8" (Error failure)
  end)
