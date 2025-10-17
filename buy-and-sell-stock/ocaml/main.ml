open Batteries
open OUnit2

module type S = sig
  val max_profit : int array -> int
end

module Impl : S = struct
  let max_profit prices =
    let f (~max_profit, ~min_price) price =
      let max_profit = Int.max (price - min_price) max_profit in
      let min_price = Int.min min_price price in
      (~max_profit, ~min_price) in
    prices
    |> Array.fold_left f (~max_profit:0, ~min_price:Int.max_num)
    |> (fun (~max_profit,..) -> max_profit)
end


let test1 _ =
  assert_equal 6 (Impl.max_profit [|10;1;5;6;7;1|])
    ~printer:String.of_int

let test2 _ =
  assert_equal 0 (Impl.max_profit [|10;8;7;5;2|])
    ~printer:String.of_int

let test3 _ =
  assert_equal 5 (Impl.max_profit [|7;1;5;3;6;4|])
    ~printer:String.of_int

let suite =
  "suite" >::: ["test1">::test1
               ;"test2">::test2
               ;"test3">::test3]

let _ = run_test_tt_main suite
