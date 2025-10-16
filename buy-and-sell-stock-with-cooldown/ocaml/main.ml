open Batteries
open OUnit2


let max_profit prices =
  let prices = Array.of_list prices in
  let len = Array.length prices in
  let max_price = Array.max prices in
  let rec cache_with_stock =
    lazy(Array.init (len + 2) (fun i -> Array.init (max_price + 1)
                                          (fun price -> lazy (loop_with_stock i price))))
  and cache_w_ref i prevprice =
    Lazy.force ((Lazy.force cache_with_stock).(i).(prevprice))
  and cache_without_stock =
    lazy(Array.init (len + 2) (fun i -> lazy (loop_without_stock i)))
  and cache_wo_ref i =
    Lazy.force ((Lazy.force cache_without_stock).(i))
  and loop_with_stock i prevprice  =
    if i >= len
    then 0
    else let price = prices.(i) in
         let sell_max = (price - prevprice) + cache_wo_ref (i + 2) in
         let notsell_max = cache_w_ref (i + 1) prevprice in
         Int.max sell_max notsell_max
  and loop_without_stock i =
    if i >= len
    then 0
    else let price = prices.(i) in
         Int.max (cache_w_ref (i + 1) price) (cache_wo_ref (i + 1)) in
  loop_without_stock 0


let test1 _ =
  assert_equal 6 (max_profit [1;3;4;0;4]) ~printer:String.of_int
let test2 _ =
  assert_equal 0 (max_profit [1]) ~printer:String.of_int
let test3 _ =
  assert_equal 3 (max_profit [1;2;3;0;2]) ~printer:String.of_int
let test4 _ =
  assert_equal 19 (max_profit [4;3;2;10;11;0;11]) ~printer:String.of_int

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2
    ;"test3" >:: test3
    ;"test4" >:: test4]

let _ = run_test_tt_main suite
