open OUnit2

module type S = sig
  val max_subarray : int list -> int
end

module Impl : S = struct
  let max_subarray nums =
    match nums with
    | [] -> raise (Invalid_argument "empty list")
    | num::nums ->
       let f (~max, ~curr) i =
         let new_curr = i + curr in
         if i > new_curr then
           (~max:(Int.max max i), ~curr:i)
         else (~max:(Int.max max new_curr), ~curr:new_curr) in
       nums
       |> List.fold_left f (~max:num,~curr:num)
       |> fun (~max,..) -> max
end

let test1 _ =
  assert_equal 8 (Impl.max_subarray [2;-3;4;-2;2;1;-1;4])
let test2 _ =
  assert_equal (-1) (Impl.max_subarray [-1])
let suite = "suite" >::: ["test1">::test1;"test2">::test2]
let _ = run_test_tt_main suite
