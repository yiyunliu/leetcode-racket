open Batteries
open OUnit2


let bsearch nums target =
  let rec loop lo hi =
    if lo >= hi then None
    else
      let mid = lo + (hi - lo) / 2 in
      let num = nums.(mid) in
      if num > target then
        loop lo mid
      else if num = target then
        Some mid
      else
        loop (mid + 1) hi in
  loop 0 (Array.length nums)

let test1 _ =
  assert_equal (Some 3) (bsearch [|-1;0;2;4;6;8|] 4)

let test2 _ =
  assert_equal None (bsearch [|-1;0;2;4;6;8|] 3)

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let _ = run_test_tt_main suite
