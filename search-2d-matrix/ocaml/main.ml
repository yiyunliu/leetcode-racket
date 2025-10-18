open Batteries
open OUnit2


let search_matrix k (m : int array array) =
  match Array.bsearch
          (fun x y -> BatOrd.ord Int.compare (x.(0)) (y.(0))) m [|k|] with
  | `All_bigger -> false
  | `All_lower ->
     begin
       match Array.bsearch (BatOrd.ord Int.compare) (m.(Array.length m - 1)) k with
       | `At _ -> true
       | _ -> false
     end
  | `At _ -> true
  | `Empty -> false
  | `Just_after i ->
     match Array.bsearch (BatOrd.ord Int.compare) (m.(i)) k with
     | `At _ -> true
     | _ -> false


let matrix =
  [|[|1;2;4;8|]
  ; [|10;11;12;13|]
  ; [|14;20;30;40|]|]

let test1 _ =
  assert_equal false (search_matrix 15 matrix)

let test2 _ =
  assert_equal true (search_matrix 20 matrix)

let test3 _ =
  assert_equal false (search_matrix 25 matrix)

let test4 _ =
  assert_equal true (search_matrix 10 matrix)

let suite =
  "suite" >:::
    ["test1">::test1
    ;"test2">::test2
    ;"test3">::test3
    ;"test4">::test4]

let _ = run_test_tt_main suite
