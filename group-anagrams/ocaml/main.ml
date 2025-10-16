open OUnit2
open Batteries

let sorted_array_of_string str =
  let arr = str |> String.enum |> Array.of_enum in
  Array.fast_sort compare arr; arr

let group_anagrams strs =
  let strs = Enum.map (fun str -> (sorted_array_of_string str, str)) strs in
  Enum.fold (fun m (sorted_str,str) ->
      m |> BatMap.modify_def [] sorted_str (fun xs -> str::xs)) BatMap.empty strs
  |> Map.values

let group_anagrams_list strs =
  strs |> List.enum |> group_anagrams |> List.of_enum

let test1 _ =
  assert_equal (List.sort compare [["hat"];["cat";"act"];["stop";"tops";"pots"]])
    (List.sort compare (group_anagrams_list ["act";"pots";"tops";"cat";"stop";"hat"]))
    ~printer:[%derive.show: string list list]

let suite =
  "suite" >:::
    ["test1" >:: test1]

let _ = run_test_tt_main suite
