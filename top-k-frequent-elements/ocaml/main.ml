open Batteries
open OUnit2

module FreqOrd = struct
  type t = int * int
  let compare (_,freq0) (_,freq1) =
    compare freq0 freq1
end

module FreqHeap = Heap.Make(FreqOrd)

let top_k_elements k nums =
  let freqs = Enum.fold (fun freqs n ->
                  Map.modify_def 0 n (fun x -> x + 1) freqs)
                Map.empty nums in
  let f heap freq =
    if FreqHeap.size heap < k then FreqHeap.add freq heap
    else let min = FreqHeap.find_min heap in
         if FreqOrd.compare freq min > 0 then
           FreqHeap.add freq (FreqHeap.del_min heap)
         else heap in
  freqs
  |> Map.enum
  |> Enum.fold f FreqHeap.empty
  |> FreqHeap.enum
  |> Enum.map Tuple2.first

let make_test expected nums k =
  fun _ -> assert_equal expected
             (List.of_enum (top_k_elements k (List.enum nums)))
             ~printer:[%derive.show: int list]

let test1 = make_test [2;3] [1;2;2;3;3;3] 2
let test2 = make_test [7] [7;7] 1

let suite =
  "suite" >::: ["test1" >:: test1; "test2" >:: test2]
let _ = run_test_tt_main suite
