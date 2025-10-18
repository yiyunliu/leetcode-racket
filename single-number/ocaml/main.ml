open OUnit2

let single_number =
  List.fold_left (lxor) 0

let test1 _ =
  assert_equal 2 (single_number [3;2;3])

let test2 _ =
  assert_equal 8 (single_number [7;6;6;7;8])

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let _ = run_test_tt_main suite
