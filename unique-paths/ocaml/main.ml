open OUnit2

module type UNIQUEPATHS = sig
  val unique_paths : int -> int -> int
end


module UniquePaths : UNIQUEPATHS = struct
  let unique_paths num_rows num_cols =
    let rec cache =
      lazy(Array.init num_rows
             (fun row -> Array.init num_cols
                           (fun col -> lazy(go row col))))
    and cache_ref i j = Lazy.force ((Lazy.force cache).(i).(j))
    and go row col =
      if row = 0 || col = 0 then 1
      else cache_ref (row - 1) col + cache_ref row (col - 1) in
    go (num_rows - 1) (num_cols - 1)
end

open UniquePaths

let test1 _ =
  assert_equal 21 (unique_paths 3 6)

let test2 _ =
  assert_equal 6 (unique_paths 3 3)


let suite = "suite" >::: ["test1">::test1 ;"test2">::test2]
let _ = run_test_tt_main suite
