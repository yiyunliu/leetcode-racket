open Batteries
open OUnit2


let two_sum (xs : int list) target =
  let tbl : (int,int) Hashtbl.t = Hashtbl.create (List.length xs) in
  let rec loop i xs =
    match xs with
    | [] -> None
    | x::xs ->
       match Hashtbl.find_opt tbl (target - x) with
       | Some y -> Some (y,i)
       | None ->
          Hashtbl.add tbl x i;
          loop (i + 1) xs in
  loop 0 xs



let test1 _ = two_sum [3;4;5;6] 7 |> assert_equal (Some (0,1))
let test2 _ = two_sum [4;5;6] 10 |> assert_equal (Some (0,2))
let test3 _ = two_sum [5;5] 10 |> assert_equal (Some (0,1))

let suite =
  "suite" >:::
    ["test1" >:: test1;
     "test2" >:: test2;
     "test3" >:: test3]

let _ = run_test_tt_main suite
