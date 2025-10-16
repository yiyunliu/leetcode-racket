open Batteries
open OUnit2

module type S = sig
  val is_palindrome : string -> bool
end

module Impl : S = struct
  let valid_char ch =
    Char.is_digit ch || Char.is_letter ch

  let is_palindrome str =

    let rec palindrome ~lo ~hi =
      lo >= hi ||
        begin let ch_f = str.[lo] in
              let ch_u = str.[hi] in
              if not (valid_char ch_f) then
                palindrome ~lo:(lo + 1) ~hi
              else if not (valid_char ch_u) then
                palindrome ~lo ~hi:(hi - 1)
              else
                Char.lowercase_ascii ch_f = Char.lowercase_ascii ch_u
                && palindrome ~lo:(lo + 1) ~hi:(hi - 1)
        end in
    palindrome ~lo:0 ~hi:(String.length str - 1)
end

open Impl

let test1 _ =
  assert_equal true (is_palindrome "Was it a car or a cat I saw?") ~printer:string_of_bool

let test2 _ =
  assert_equal false (is_palindrome "ab") ~printer:string_of_bool

let test3 _ =
  assert_equal true (is_palindrome "bb") ~printer:string_of_bool

let test4 _ =
  assert_equal true (is_palindrome "a!!b??a") ~printer:string_of_bool

let test5 _ =
  assert_equal true (is_palindrome "") ~printer:string_of_bool

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2
    ;"test3" >:: test3
    ;"test4" >:: test4
    ;"test5" >:: test5]

let _ = run_test_tt_main suite
