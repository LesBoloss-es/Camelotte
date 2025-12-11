open Alcotest

let test_basic_casts () =
  let open NextGWrite in
  let test cast value expected = check Alcotest.string expected expected (to_string cast value) in
  test int 7 "7";
  test bit true "1";
  test bit false "0";
  test float 34.2 "34.2";
  test char 'Y' "Y";
  test string "Bonjour" "Bonjour";
  test (list int) [1; 2; 7] "1 2 7";
  test (list int) [] "";
  test (array int) [|1; 2; 7|] "1 2 7";
  test (array int) [||] "";
  test (pair int) (7, 8) "7 8";
  test (pairg int float) (7, 34.2) "7 34.2";
  test (tuple2 int) (8, 9) "8 9";
  test (tuple2g int float) (7, 34.2) "7 34.2";
  test (tuple3 int) (7, 8, 9) "7 8 9";
  test (tuple3g int float string) (7, 8., "9") "7 8 9";
  test (tuple3g int float string) (7, 8., "9 10") "7 8 9 10";
  test (tuple4 int) (7, 8, 9, 10) "7 8 9 10";
  test (tuple4g int float char string) (7, 8., '9', "10") "7 8 9 10";
  test (tuple4g int float char string) (7, 8., '9', "10 11") "7 8 9 10 11";
  test (tuple5 int) (7, 8, 9, 10, 11) "7 8 9 10 11";
  test (tuple5g int float bit char string) (7, 8., true, '9', "10") "7 8 1 9 10";
  test (tuple5g int float bit char string) (7, 8., false, '9', "10 11") "7 8 0 9 10 11"

let tests = [
  "basic_casts", [test_case "basic cast operations" `Quick test_basic_casts];
]
