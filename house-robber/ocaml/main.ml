open Batteries
open OUnit2

let rob (nums : int list) : int =
  let nums = Array.of_list nums in
  let len = Array.length nums in
  let rec cache = lazy (Array.init (len + 1)
                          (fun i -> lazy (go i)))
  and cache_ref i = Lazy.force ((Lazy.force cache).(i))
  and go i =
    match i with
    | 0 -> 0
    | 1 -> nums.(0)
    | i -> Int.max (cache_ref (i - 2) + nums.(i - 1)) (cache_ref (i - 1)) in
  go len

let test1 _ = rob [1;1;3;3] |> assert_equal 4
let test2 _ = rob [2;9;8;3;6] |> assert_equal 16
let test3 _ = rob [2;9;8;3] |> assert_equal 12
let test4 _ = rob [9;8;3;6] |> assert_equal 15

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2
    ;"test3" >:: test3
    ;"test4" >:: test4]

let _ = run_test_tt_main suite
