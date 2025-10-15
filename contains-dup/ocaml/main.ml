open Batteries
open OUnit2

module type S = sig
  val hasDuplicate : int list -> bool
end


module I : S = struct
  let hasDuplicate xs =
    let rec loop xs s =
      match xs with
      | [] -> false
      | (x::xs) ->
         Set.mem x s || loop xs (Set.add x s)
    in loop xs Set.empty
end

let test1 _ = assert_equal false (I.hasDuplicate [1;2;3;5])
let test2 _ = assert_equal true (I.hasDuplicate [1;4;5;4])

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let () =
  run_test_tt_main suite
