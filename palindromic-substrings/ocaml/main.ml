open OUnit2

module type S = sig
  val num_palindromic_substrs : string -> int
end

module Impl : S = struct
  let num_palindromic_substrs s =
    let len = String.length s in
    let rec cache = lazy(Array.init len (fun lo -> Array.init (len + 1) (fun hi -> (lazy (palindrome lo hi)))))
    and cache_ref lo hi = Lazy.force ((Lazy.force cache).(lo).(hi))
    and palindrome i j =
      if i + 1 >= j
      (* j - i <= 1 *)
      then true
      (* j - 1 >= 2 *)
      else let lo = s.[i] in
           let hi = s.[j-1] in
           lo = hi && cache_ref (i + 1) (j - 1) in
    BatEnum.range 0 ~until:(len-1)
    |> BatEnum.concat_map
         (fun lo ->
           BatEnum.range (lo + 1) ~until:len
           |> BatEnum.filter
                (fun hi -> palindrome lo hi))
    |> BatEnum.count
end

let test1 _ =
  assert_equal 3 (Impl.num_palindromic_substrs "abc") ~printer:string_of_int

let test2 _ =
  assert_equal 6 (Impl.num_palindromic_substrs "aaa") ~printer:string_of_int

let test3 _ =
  assert_equal 500500 (Impl.num_palindromic_substrs (String.make 1000 'a')) ~printer:string_of_int


let suite =
  "suite" >::: ["test1" >:: test1; "test2" >:: test2; "test3" >:: test3]

let _ = run_test_tt_main suite
