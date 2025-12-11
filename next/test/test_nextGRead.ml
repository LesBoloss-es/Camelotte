open Alcotest

let result_testable ok_t =
  let pp fmt = function
    | Ok v -> Fmt.pf fmt "Ok(%a)" (Alcotest.pp ok_t) v
    | Error e -> Fmt.pf fmt "Error(%s)" (Printexc.to_string e)
  in
  let equal r1 r2 =
    match r1, r2 with
    | Ok v1, Ok v2 -> Alcotest.equal ok_t v1 v2
    | Error e1, Error e2 ->
      (
        match e1, e2 with
        | Failure _, Failure _ -> true
        | Invalid_argument _, Invalid_argument _ -> true
        | _ -> false
      )
    | _ -> false
  in
  testable pp equal

let test_basic_parsers () =
  let open NextGRead in
  let test cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable Alcotest.int) str expected result
  in
  let test_float cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.float 0.001)) str expected result
  in
  let test_char cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable Alcotest.char) str expected result
  in
  let test_string cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable Alcotest.string) str expected result
  in
  let test_bool cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable Alcotest.bool) str expected result
  in
  test int "7" (Ok 7);
  test int "L" (Error (Failure ""));
  test_bool bit "1" (Ok true);
  test_bool bit "0" (Ok false);
  test_bool bit "T" (Error (Failure ""));
  test_float float "34.2" (Ok 34.2);
  test_float float "TRUE" (Error (Failure ""));
  test_char char "Y" (Ok 'Y');
  test_char char "YO" (Error (Failure ""));
  test_string string "Bonjour" (Ok "Bonjour");
  test_string no_space_string "Bonjour" (Ok "Bonjour");
  test_string no_space_string "Bon jour" (Error (Failure ""))

let test_list_array_parsers () =
  let open NextGRead in
  let test_list cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.list Alcotest.int)) str expected result
  in
  let test_array cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.array Alcotest.int)) str expected result
  in
  test_list (list int) "1 2 7" (Ok [1; 2; 7]);
  test_list (list int) "1 L 7" (Error (Failure ""));
  test_list (list int) "" (Ok []);
  test_list (non_empty_list int) "" (Error (Failure ""));
  test_array (array int) "1 2 7" (Ok [|1; 2; 7|]);
  test_array (array int) "1 L 7" (Error (Failure ""));
  test_array (array int) "" (Ok [||]);
  test_array (non_empty_array int) "" (Error (Failure ""))

let test_tuple_parsers () =
  let open NextGRead in
  let test_pair cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.pair Alcotest.int Alcotest.int)) str expected result
  in
  let test_pair_mixed cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.pair Alcotest.int (Alcotest.float 0.001))) str expected result
  in
  test_pair (pair int int) "7 8" (Ok (7, 8));
  test_pair (pair int int) "7 L" (Error (Failure ""));
  test_pair (pair int int) "7" (Error (Failure ""));
  test_pair (pair int int) "7 8 9" (Error (Failure ""));
  test_pair_mixed (pair int float) "7 34.2" (Ok (7, 34.2));
  test_pair (tuple2 int int) "8 9" (Ok (8, 9));
  test_pair_mixed (tuple2 int float) "7 34.2" (Ok (7, 34.2))

let test_tuple3_parsers () =
  let open NextGRead in
  let test_tuple3 cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) str expected result
  in
  let test_tuple3_mixed cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let triple_testable =
      Alcotest.testable
        (fun fmt (a, b, c) -> Fmt.pf fmt "(%d, %g, %s)" a b c)
        (fun (a1, b1, c1) (a2, b2, c2) -> a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2)
    in
    check (result_testable triple_testable) str expected result
  in
  test_tuple3 (tuple3 int int int) "7 8 9" (Ok (7, 8, 9));
  test_tuple3 (tuple3 int int int) "7 L 9" (Error (Failure ""));
  test_tuple3 (tuple3 int int int) "7 8" (Error (Failure ""));
  test_tuple3 (tuple3 int int int) "7 8 9 10" (Error (Failure ""));
  test_tuple3_mixed (tuple3 int float string) "7 8 9" (Ok (7, 8., "9"));
  test_tuple3_mixed (tuple3 int float string) "7 8" (Error (Failure ""));
  test_tuple3_mixed (tuple3 int float string) "7 8 9 10" (Ok (7, 8., "9 10"));
  test_tuple3_mixed (tuple3 int float no_space_string) "7 8 9 10" (Error (Failure ""))

let test_tuple4_parsers () =
  let open NextGRead in
  let test_tuple4 cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let tuple4_testable =
      Alcotest.testable
        (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %d, %d, %d)" a b c d)
        (=)
    in
    check (result_testable tuple4_testable) str expected result
  in
  let test_tuple4_mixed cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let tuple4_testable =
      Alcotest.testable
        (fun fmt (a, b, c, d) -> Fmt.pf fmt "(%d, %g, %c, %s)" a b c d)
        (fun (a1, b1, c1, d1) (a2, b2, c2, d2) ->
          a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2
        )
    in
    check (result_testable tuple4_testable) str expected result
  in
  test_tuple4 (tuple4 int int int int) "7 8 9 10" (Ok (7, 8, 9, 10));
  test_tuple4_mixed (tuple4 int float char string) "7 8 9 10" (Ok (7, 8., '9', "10"));
  test_tuple4_mixed (tuple4 int float char string) "7 8 9 10 11" (Ok (7, 8., '9', "10 11"));
  test_tuple4_mixed (tuple4 int float char no_space_string) "7 8 9 10 11" (Error (Failure ""))

let test_tuple5_parsers () =
  let open NextGRead in
  let test_tuple5 cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let tuple5_testable =
      Alcotest.testable
        (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %d, %d, %d, %d)" a b c d e)
        (=)
    in
    check (result_testable tuple5_testable) str expected result
  in
  let test_tuple5_mixed cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let tuple5_testable =
      Alcotest.testable
        (fun fmt (a, b, c, d, e) -> Fmt.pf fmt "(%d, %g, %b, %c, %s)" a b c d e)
        (fun (a1, b1, c1, d1, e1) (a2, b2, c2, d2, e2) ->
          a1 = a2 && Float.abs (b1 -. b2) < 0.001 && c1 = c2 && d1 = d2 && e1 = e2
        )
    in
    check (result_testable tuple5_testable) str expected result
  in
  test_tuple5 (tuple5 int int int int int) "7 8 9 10 11" (Ok (7, 8, 9, 10, 11));
  test_tuple5_mixed (tuple5 int float bit char string) "7 8 1 9 10" (Ok (7, 8., true, '9', "10"));
  test_tuple5_mixed (tuple5 int float bit char string) "7 8 0 9 10 11" (Ok (7, 8., false, '9', "10 11"));
  test_tuple5_mixed (tuple5 int float bit char no_space_string) "7 8 0 9 10 11" (Error (Failure ""))

let test_nested_parsers () =
  let open NextGRead in
  let test_nested cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let nested_testable =
      Alcotest.testable
        (fun fmt (a, lst) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.list string) lst)
        (=)
    in
    check (result_testable nested_testable) str expected result
  in
  let test_nested_array cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let nested_testable =
      Alcotest.testable
        (fun fmt (a, arr) -> Fmt.pf fmt "(%d, %a)" a Fmt.(Dump.array string) arr)
        (=)
    in
    check (result_testable nested_testable) str expected result
  in
  test_nested (pair int (list string)) "7 8 9 10" (Ok (7, ["8"; "9"; "10"]));
  test_nested (pair int (list string)) "7" (Error (Failure ""));
  test_nested_array (pair int (array string)) "7 8 9 10" (Ok (7, [|"8"; "9"; "10"|]));
  test_nested_array (pair int (array string)) "7" (Error (Failure ""))

let test_custom_separators () =
  let open NextGRead in
  let comma = Str.regexp "[ \t]*,[ \t]*" in
  let dash = Str.regexp "-" in
  let test_list cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.list Alcotest.int)) str expected result
  in
  let test_triple cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    check (result_testable (Alcotest.triple Alcotest.int Alcotest.int Alcotest.int)) str expected result
  in
  let test_nested cast str expected =
    let result = try Ok (of_string cast str) with exn -> Error exn in
    let nested_testable =
      Alcotest.testable
        (fun fmt lst -> Fmt.pf fmt "%a" Fmt.(Dump.list (pair int float)) lst)
        (=)
    in
    check (result_testable nested_testable) str expected result
  in
  test_list (list ~sep: comma int) "1,2,7" (Ok [1; 2; 7]);
  test_list (list ~sep: comma int) "1, 2 ,7" (Ok [1; 2; 7]);
  test_list (list ~sep: comma int) "1   , 2 , 7" (Ok [1; 2; 7]);
  test_list (list ~sep: comma int) "1  , , 2 , 7" (Error (Failure ""));
  test_triple (triple ~sep: comma int int int) "1,2,7" (Ok (1, 2, 7));
  test_triple (triple ~sep: comma int int int) "1, 2 ,7" (Ok (1, 2, 7));
  test_triple (triple ~sep: comma int int int) "1   , 2 , 7" (Ok (1, 2, 7));
  test_triple (triple ~sep: comma int int int) "1  , , 2 , 7" (Error (Failure ""));
  test_nested (list ~sep: comma (pair ~sep: dash int float)) "2-4,6-8" (Ok [(2, 4.); (6, 8.)]);
  test_nested (list ~sep: comma (pair ~sep: dash int float)) "2-4, 6-8" (Ok [(2, 4.); (6, 8.)]);
  test_nested (list ~sep: comma (pair ~sep: dash int float)) "2-4  ,6-8" (Ok [(2, 4.); (6, 8.)]);
  test_nested (list ~sep: comma (pair ~sep: dash int float)) "2-4-3  ,6-8" (Error (Failure ""));
  test_nested (list ~sep: comma (pair ~sep: dash int float)) "2,4-5  ,6-8" (Error (Failure ""))

let tests = [
  "basic_parsers", [test_case "int, float, char, string, bit" `Quick test_basic_parsers];
  "list_array", [test_case "list and array parsing" `Quick test_list_array_parsers];
  "tuple2", [test_case "pair/tuple2 parsing" `Quick test_tuple_parsers];
  "tuple3", [test_case "tuple3/triple parsing" `Quick test_tuple3_parsers];
  "tuple4", [test_case "tuple4 parsing" `Quick test_tuple4_parsers];
  "tuple5", [test_case "tuple5 parsing" `Quick test_tuple5_parsers];
  "nested", [test_case "nested structures" `Quick test_nested_parsers];
  "separators", [test_case "custom separators" `Quick test_custom_separators];
]
