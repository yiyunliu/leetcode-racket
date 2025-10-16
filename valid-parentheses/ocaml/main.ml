open Batteries
open OUnit2

let get_open_paren_opt ch =
  match ch with
  | ')' -> Some '('
  | ']' -> Some '['
  | '}' -> Some '{'
  | _ -> None

let valid_parens str =
  let stack = Stack.create () in
  let len = String.length str in
  let rec loop i =
    i >= len ||
      let ch = str.[i] in
      match get_open_paren_opt ch with
      | None ->
         Stack.push ch stack;
         loop (i + 1)
      | Some och ->
         not (Stack.is_empty stack) &&
           Stack.pop stack = och && loop (i + 1) in
  loop 0

let test1 _ = assert_equal true (valid_parens "[]") ~printer: string_of_bool
let test2 _ = assert_equal true (valid_parens "([{}])") ~printer: string_of_bool
let test3 _ = assert_equal false (valid_parens "[(])") ~printer: string_of_bool

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2
    ;"test3" >:: test3]

let _ = run_test_tt_main suite
