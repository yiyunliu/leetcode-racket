open Batteries
open OUnit2


module type S = sig
  val is_anagram : string -> string -> bool
end

module I : S = struct
  let char_to_index =
    let ind_a = int_of_char 'a' in
    fun x -> int_of_char x - ind_a

  let string_to_count str =
    let arr = Array.make 26 0 in
    String.iter
      (fun x ->
        let i = char_to_index x in
        arr.(i) <- arr.(i) + 1) str;
    arr

  let is_anagram str0 str1 =
    string_to_count str0 = string_to_count str1
end

let test1 _ = assert_equal true (I.is_anagram "racecar" "carrace")
let test2 _ = assert_equal false (I.is_anagram "jar" "jam")

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]


let () =
  run_test_tt_main suite
