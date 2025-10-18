open OUnit2

let trap (heights : int array) : int =
  let rec loop lo left_height hi right_height acc =
    if lo > hi then
      acc
    else if left_height < right_height then
      let height = heights.(lo) in
      loop (lo + 1) (Int.max left_height height) hi right_height
        (acc + (Int.max (left_height - height) 0))
    else
      let height = heights.(hi) in
      loop lo left_height (hi - 1) (Int.max right_height height)
        (acc + (Int.max (right_height - height) 0))
  in
  let last = Array.length heights - 1 in
  loop 0 0 last 0 0



let test1 _ =
  assert_equal 9 (trap [|0;2;0;3;1;0;1;4;2;1|])
    ~printer:string_of_int

let test2 _ =
  assert_equal 1 (trap [|1;0;1|])
    ~printer:string_of_int


let suite =
  "suite" >::: ["test1" >:: test1
               ;"test2" >:: test2]

let _ = run_test_tt_main suite
