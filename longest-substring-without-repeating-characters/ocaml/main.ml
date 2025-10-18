open OUnit2

let length_of_longest_substr (str : string) =
  let index_of_char = Hashtbl.create (String.length str) in
  let len = String.length str in
  let rec loop max_len lo hi =
    if hi >= len then (Int.max max_len (hi - lo))
    else let ch = str.[hi] in
         let prev_idx = BatHashtbl.find_default index_of_char ch ~-1 in
         Hashtbl.add index_of_char ch hi;
         if prev_idx >= lo then
           (* repeating character detected! *)
           loop (Int.max max_len (hi - lo)) (prev_idx + 1) (hi + 1)
         else
           loop max_len lo (hi + 1) in
  loop 0 0 0

let test1 _ =
  assert_equal 3 (length_of_longest_substr "zxyzxyz")
    ~printer:string_of_int

let test2 _ =
  assert_equal 1 (length_of_longest_substr "xxxx")
    ~printer:string_of_int

let test3 _ =
  assert_equal 3 (length_of_longest_substr "abcabcbb")
    ~printer:string_of_int

let test4 _ =
  assert_equal 3 (length_of_longest_substr "pwwkew")
    ~printer:string_of_int

let suite =
  "suite" >:::
    ["test1">::test1
    ;"test2">::test2
    ;"test3">::test3
    ;"test4">::test4]

let _ = run_test_tt_main suite
