open Alcotest

let test_split_on_char_n () =
  let open NextString in
  let s = "foo bar baz" in
  check (list string) "split into 1" ["foo bar baz"] (split_on_char_n 1 ' ' s);
  check (list string) "split into 2" ["foo"; "bar baz"] (split_on_char_n 2 ' ' s);
  check (list string) "split into 3" ["foo"; "bar"; "baz"] (split_on_char_n 3 ' ' s);
  check_raises
    "invalid n=0"
    (Invalid_argument "ExtString.split_on_char_n")
    (fun () -> ignore (split_on_char_n 0 ' ' s));
  check_raises
    "n too large"
    (Failure "ExtString.split_on_char_n")
    (fun () -> ignore (split_on_char_n 4 ' ' s));
  check (list string) "with leading space 2" [""; "foo "] (split_on_char_n 2 ' ' " foo ");
  check (list string) "with leading space 3" [""; "foo"; ""] (split_on_char_n 3 ' ' " foo ")

let tests = [
  "split_on_char_n", [test_case "split operations" `Quick test_split_on_char_n];
]
