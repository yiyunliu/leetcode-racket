open OUnit2

let rec merge xs ys =
  match xs, ys with
  | [],ys -> ys
  | xs,[] -> xs
  | x::xs0,y::ys0 ->
     if x < y then
       x :: (merge xs0 ys)
     else y :: (merge xs ys0)


let test1 _ =
  assert_equal [1;1;2;3;4;5] (merge [1;2;4] [1;3;5])

let suite =
  "suite" >:::
    ["test1">::test1

let _ = run_test_tt_main suite
