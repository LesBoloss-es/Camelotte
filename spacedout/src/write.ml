type 'a cast = 'a -> string

(* Writers *)

let string_as cast v = cast v

let line_as cast ochan v =
  output_string ochan (string_as cast v);
  output_char ochan '\n'

(* Casts *)

let int = string_of_int
let bit = function true -> "1" | _ -> "0"
let float = Format.sprintf "%g"
let char = String.make 1
let string = Fun.id

let blank = " "

let list ?(sep = blank) c l = List.map c l |> String.concat sep
let seq ?sep c s = list ?sep c (List.of_seq s)
let array ?sep c a = list ?sep c (Array.to_list a)

let tuple_2 ?(sep = blank) c1 c2 (v1, v2) =
  c1 v1 ^ sep ^ c2 v2

let pair = tuple_2
let couple = tuple_2

let tuple_3 ?(sep = blank) c1 c2 c3 (v1, v2, v3) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3

let triple = tuple_3

let tuple_4 ?(sep = blank) c1 c2 c3 c4 (v1, v2, v3, v4) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4

let quadruple = tuple_4

let tuple_5 ?(sep = blank) c1 c2 c3 c4 c5 (v1, v2, v3, v4, v5) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5

let pentuple = tuple_5
let quintuple = tuple_5

let tuple_6 ?(sep = blank) c1 c2 c3 c4 c5 c6 (v1, v2, v3, v4, v5, v6) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6

let sextuple = tuple_6
let hextuple = tuple_6

let tuple_7 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 (v1, v2, v3, v4, v5, v6, v7) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7

let septuple = tuple_7
let heptuple = tuple_7

let tuple_8 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 (v1, v2, v3, v4, v5, v6, v7, v8) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7 ^ sep ^ c8 v8

let octuple = tuple_8

let tuple_9 ?(sep = blank) c1 c2 c3 c4 c5 c6 c7 c8 c9 (v1, v2, v3, v4, v5, v6, v7, v8, v9) =
  c1 v1 ^ sep ^ c2 v2 ^ sep ^ c3 v3 ^ sep ^ c4 v4 ^ sep ^ c5 v5 ^ sep ^ c6 v6 ^ sep ^ c7 v7 ^ sep ^ c8 v8 ^ sep ^ c9 v9

let nonuple = tuple_9

(* Tests *)

let%test_module _ =
  (module struct
    let test cast value expected =
      string_as cast value = expected

    let%test _ = test int 7 "7"

    let%test _ = test bit true "1"
    let%test _ = test bit false "0"

    let%test _ = test float 34.2 "34.2"

    let%test _ = test char 'Y' "Y"

    let%test _ = test string "Bonjour" "Bonjour"

    let%test _ = test (list int) [1; 2; 7] "1 2 7"
    let%test _ = test (list int) [] ""

    let%test _ = test (array int) [|1; 2; 7|] "1 2 7"
    let%test _ = test (array int) [||] ""

    let%test _ = test (pair int int) (7, 8) "7 8"
    let%test _ = test (pair int float) (7, 34.2) "7 34.2"
    let%test _ = test (tuple_2 int int) (8, 9) "8 9"
    let%test _ = test (tuple_2 int float) (7, 34.2) "7 34.2"

    let%test _ = test (tuple_3 int int int) (7, 8, 9) "7 8 9"
    let%test _ = test (tuple_3 int float string) (7, 8., "9") "7 8 9"
    let%test _ = test (tuple_3 int float string) (7, 8., "9 10") "7 8 9 10"

    let%test _ = test (tuple_4 int int int int) (7, 8, 9, 10) "7 8 9 10"
    let%test _ = test (tuple_4 int float char string) (7, 8., '9', "10") "7 8 9 10"
    let%test _ = test (tuple_4 int float char string) (7, 8., '9', "10 11") "7 8 9 10 11"

    let%test _ = test (tuple_5 int int int int int) (7, 8, 9, 10, 11) "7 8 9 10 11"
    let%test _ = test (tuple_5 int float bit char string) (7, 8., true, '9', "10") "7 8 1 9 10"
    let%test _ = test (tuple_5 int float bit char string) (7, 8., false, '9', "10 11") "7 8 0 9 10 11"
  end)
