open Alcotest

let tests = [
  ("NextList", Test_nextList.tests);
  ("NextString", Test_nextString.tests);
  ("NextGWrite", Test_nextGWrite.tests);
  ("NextGRead", Test_nextGRead.tests);
]

let () =
  run "Next" @@
    List.concat_map
      (fun (module_name, tests) ->
        List.map
          (fun (function_name, test) ->
            (Format.sprintf "%s.%s" module_name function_name, test)
          )
          tests
      )
      tests
