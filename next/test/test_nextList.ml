open Alcotest

let test_union_sorted () =
  let open NextList in
  let union_sorted = union_sorted Int.compare in
  let l1 = [1; 3; 6] in
  let l2 = [1; 2; 4; 5] in
  let l12 = [1; 2; 3; 4; 5; 6] in
  check (list int) "empty lists" [] (union_sorted [] []);
  check (list int) "l1 with empty" l1 (union_sorted l1 []);
  check (list int) "empty with l1" l1 (union_sorted [] l1);
  check (list int) "l1 with itself" l1 (union_sorted l1 l1);
  check (list int) "l1 union l2" l12 (union_sorted l1 l2);
  check (list int) "l2 union l1" l12 (union_sorted l2 l1)

let test_inter_sorted () =
  let open NextList in
  let f = inter_sorted Int.compare in
  let l1 = [1; 3; 4; 6] in
  let l2 = [1; 2; 4; 5] in
  let l12 = [1; 4] in
  check (list int) "empty lists" [] (f [] []);
  check (list int) "l1 with empty" [] (f l1 []);
  check (list int) "empty with l1" [] (f [] l1);
  check (list int) "l1 with itself" l1 (f l1 l1);
  check (list int) "l1 inter l2" l12 (f l1 l2);
  check (list int) "l2 inter l1" l12 (f l2 l1)

let test_diff_sorted () =
  let open NextList in
  let f = diff_sorted Int.compare in
  let l1 = [1; 3; 4; 6] in
  let l2 = [1; 2; 4; 5] in
  let l12 = [3; 6] in
  let l21 = [2; 5] in
  check (list int) "empty lists" [] (f [] []);
  check (list int) "l1 diff empty" l1 (f l1 []);
  check (list int) "empty diff l1" [] (f [] l1);
  check (list int) "l1 diff itself" [] (f l1 l1);
  check (list int) "l1 diff l2" l12 (f l1 l2);
  check (list int) "l2 diff l1" l21 (f l2 l1)

let test_symdiff_sorted () =
  let open NextList in
  let f = symdiff_sorted Int.compare in
  let l1 = [1; 3; 4; 6] in
  let l2 = [1; 2; 4; 5] in
  let l12 = [2; 3; 5; 6] in
  check (list int) "empty lists" [] (f [] []);
  check (list int) "l1 with empty" l1 (f l1 []);
  check (list int) "empty with l1" l1 (f [] l1);
  check (list int) "l1 with itself" [] (f l1 l1);
  check (list int) "l1 symdiff l2" l12 (f l1 l2);
  check (list int) "l2 symdiff l1" l12 (f l2 l1)

let test_sorted_property_based () =
  let open NextList in
  let gen ~max size =
    List.init size (fun _ -> Random.int (max + 1))
    |> List.sort_uniq Int.compare
  in
  let rec test ~repeat ~max s1 s2 f g =
    if repeat <= 0 then true
    else
      let l1 = gen ~max s1 in
      let l2 = gen ~max s2 in
      (f Int.compare l1 l2 = g Int.compare l1 l2)
      && test ~repeat: (repeat - 1) ~max s1 s2 f g
  in

  (* union_sorted *)
  let f = union_sorted in
  let g cmp l1 l2 = List.sort_uniq cmp (l1 @ l2) in
  check bool "union vs spec 1" true (test ~repeat: 100 ~max: 10 20 18 f g);
  check bool "union vs spec 2" true (test ~repeat: 100 ~max: 10 18 20 f g);
  check bool "union vs spec 3" true (test ~repeat: 100 ~max: 100 200 180 f g);
  check bool "union vs spec 4" true (test ~repeat: 100 ~max: 100_000_000 1_800 2_000 f g);

  (* inter_sorted *)
  let f = inter_sorted in
  let g cmp l1 l2 = List.filter (fun x1 -> List.exists (fun x2 -> cmp x1 x2 = 0) l2) l1 in
  check bool "inter vs spec 1" true (test ~repeat: 100 ~max: 10 20 18 f g);
  check bool "inter vs spec 2" true (test ~repeat: 100 ~max: 10 18 20 f g);
  check bool "inter vs spec 3" true (test ~repeat: 100 ~max: 100 200 180 f g);
  check bool "inter vs spec 4" true (test ~repeat: 100 ~max: 100_000_000 1_800 2_000 f g);

  (* diff_sorted *)
  let f = diff_sorted in
  let g cmp l1 l2 = List.filter (fun x1 -> not (List.exists (fun x2 -> cmp x1 x2 = 0) l2)) l1 in
  check bool "diff vs spec 1" true (test ~repeat: 100 ~max: 10 20 18 f g);
  check bool "diff vs spec 2" true (test ~repeat: 100 ~max: 10 18 20 f g);
  check bool "diff vs spec 3" true (test ~repeat: 100 ~max: 100 200 180 f g);
  check bool "diff vs spec 4" true (test ~repeat: 100 ~max: 100_000_000 1_800 2_000 f g);

  (* symdiff_sorted *)
  let f = symdiff_sorted in
  let g cmp l1 l2 = union_sorted cmp (diff_sorted cmp l1 l2) (diff_sorted cmp l2 l1) in
  check bool "symdiff vs spec 1" true (test ~repeat: 100 ~max: 10 20 18 f g);
  check bool "symdiff vs spec 2" true (test ~repeat: 100 ~max: 10 18 20 f g);
  check bool "symdiff vs spec 3" true (test ~repeat: 100 ~max: 100 200 180 f g);
  check bool "symdiff vs spec 4" true (test ~repeat: 100 ~max: 100_000_000 1_800 2_000 f g)

let test_hdn () =
  let open NextList in
  check (list int) "hdn 0" [] (hdn 0 [1; 2; 3; 4; 5]);
  check (list int) "hdn 1" [1] (hdn 1 [1; 2; 3; 4; 5]);
  check (list int) "hdn 3" [1; 2; 3] (hdn 3 [1; 2; 3; 4; 5]);
  check (list int) "hdn 5" [1; 2; 3; 4; 5] (hdn 5 [1; 2; 3; 4; 5]);
  check (list int) "hdn 6" [1; 2; 3; 4; 5] (hdn 6 [1; 2; 3; 4; 5])

let test_sub () =
  let open NextList in
  check (list int) "sub pos 3 len 0" [] (sub [1; 2; 3; 4; 5] 3 0);
  check (list int) "sub pos 0 len 3" [1; 2; 3] (sub [1; 2; 3; 4; 5] 0 3);
  check (list int) "sub pos 2 len 2" [3; 4] (sub [1; 2; 3; 4; 5] 2 2);
  check (list int) "sub pos 1 len 4" [2; 3; 4; 5] (sub [1; 2; 3; 4; 5] 1 4);
  check (list int) "sub pos 0 len 5" [1; 2; 3; 4; 5] (sub [1; 2; 3; 4; 5] 0 5)

let tests = [
  "union_sorted", [test_case "basic operations" `Quick test_union_sorted];
  "inter_sorted", [test_case "basic operations" `Quick test_inter_sorted];
  "diff_sorted", [test_case "basic operations" `Quick test_diff_sorted];
  "symdiff_sorted", [test_case "basic operations" `Quick test_symdiff_sorted];
  "property_based", [test_case "vs specifications" `Quick test_sorted_property_based];
  "hdn", [test_case "head n elements" `Quick test_hdn];
  "sub", [test_case "sublist" `Quick test_sub];
]
