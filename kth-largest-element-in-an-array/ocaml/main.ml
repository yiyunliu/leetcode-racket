open Batteries
open OUnit2

let find_kth k nums =
  let add h i =
    let size = Heap.size h in
    if size < k then Heap.add i h
    else let m = Heap.find_min h in
         if i < m then h else  Heap.add i (Heap.del_min h) in
  nums |> Array.fold_left add Heap.empty |> Heap.find_min

let test1 _ = assert_equal 4 (find_kth 2 [|2;3;1;5;4|])
let test2 _ = assert_equal 4 (find_kth 3 [|2;3;1;1;5;5;4|])

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let _ = run_test_tt_main suite
