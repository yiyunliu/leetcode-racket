let meeting_rooms (intervals : (int * int) array) : bool =
  Array.fast_sort (fun (x,_) (y,_) -> Int.compare x y) intervals;
  let iter = BatArray.enum intervals in
  let rec loop max_end =
    match BatEnum.get iter with
    | None -> true
    | Some (start,new_end) ->
       start >= max_end && loop (Int.max max_end new_end) in
  loop 0

open OUnit2

let test1 _ =
  assert_equal false (meeting_rooms [|(0,30);(5,10);(15,20)|])

let test2 _ =
  assert_equal true (meeting_rooms [|(5,8);(9,15)|])

let test3 _ =
  assert_equal false (meeting_rooms [|(2,4);(9,12);(6,10)|])

let test4 _ =
  assert_equal true (meeting_rooms [|(1,4);(10,15);(7,10)|])

let suite =
  "suite" >:::
    ["test1">::test1
    ;"test2">::test2
    ;"test3">::test3
    ;"test4">::test4]

let _ = run_test_tt_main suite
