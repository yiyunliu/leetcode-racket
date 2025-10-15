open Batteries
open OUnit2


let two_sum (xs : int list) target =
  let xs = Array.of_list xs in
  Array.fast_sort Int.compare xs;
  let rec loop i j =
    if i >= j then None
    else
      let sum = xs.(i) + xs.(j) in
      match BatOrd.ord compare sum target with
      | BatOrd.Gt ->
         loop i (j - 1)
      | BatOrd.Eq ->
         Some (i,j)
      | BatOrd.Lt ->
         loop (i + 1) j
  in
  loop 0 (Array.length xs - 1)


let test1 _ = two_sum [3;4;5;6] 7 |> assert_equal (Some (0,1))
let test2 _ = two_sum [4;5;6] 10 |> assert_equal (Some (0,2))
let test3 _ = two_sum [5;5] 10 |> assert_equal (Some (0,1))

let suite =
  "suite" >:::
    ["test1" >:: test1;
     "test2" >:: test2;
     "test3" >:: test3]

let _ = run_test_tt_main suite
