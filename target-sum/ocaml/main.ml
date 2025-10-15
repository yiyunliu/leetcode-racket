open Batteries
open OUnit2

let target_sum target nums =
  let cache = Hashtbl.create (Array.length nums) in
  let len = Array.length nums in
  let rec cacheRef i target =
    match Hashtbl.find_opt cache (i,target) with
    | Some r -> r
    | None ->
       let result = go i target in
       Hashtbl.add cache (i,target) result;
       result
  and go i target =
    match i with
    | 0 ->  if target = 0 then 1 else 0
    | i ->
       let num = nums.(i-1) in
       let target0 = num + target in
       let target1 = num - target in
       cacheRef (i - 1) target0 + cacheRef (i - 1) target1 in
  go len target

let make_test expected target nums  =
  assert_equal expected (target_sum target nums) ~printer:string_of_int

let test0 _ = make_test 3 2 (Array.make 3 2)
let test1 _ = make_test 5 3 (Array.make 5 1)

let suite =
  "suite" >:::
    ["test0" >:: test0
    ;"test1" >:: test1]

let _ = run_test_tt_main suite
