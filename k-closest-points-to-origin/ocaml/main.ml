open Batteries
open OUnit2

module type KCLOSEST = sig
  val k_closest : int -> (int * int) Enum.t -> (int * int) Enum.t
end

module KClosest : KCLOSEST = struct
  type point = int * int

  module PointOrd = struct
    type t = point
    let compare (x0,y0) (x1,y1) =
      compare (x1 * x1 + y1 * y1) (x0 * x0 + y0 * y0)
  end

  module MaxHeap = Heap.Make(PointOrd)

  type kheap = {k : int; heap : MaxHeap.t}

  let empty k = {k; heap = MaxHeap.empty}

  let size {heap;_} = MaxHeap.size heap

  let insert p kh =
    if size kh < kh.k
    then {kh with heap = MaxHeap.add p kh.heap}
    else let m = MaxHeap.find_min kh.heap in
         if PointOrd.compare p m <= 0
         then kh
         else {kh with heap = (MaxHeap.add p (MaxHeap.del_min kh.heap))}

  let k_closest k points =
    Enum.fold (fun kh p -> insert p kh) (empty k) points
    |> (fun x -> x.heap)
    |> MaxHeap.enum
end


open KClosest

let sorted_list_of_enum xs =  xs |> BatList.of_enum |> BatList.sort compare

let test1 _ =
  assert_equal (BatList.sort compare [(2,0);(0,2)]) (List.enum [(0,2);(2,0);(2,2)] |> k_closest 2 |> sorted_list_of_enum)

let test2 _ =
  assert_equal (BatList.sort compare [(0,2)]) (List.enum [(0,2);(2,2)] |> k_closest 1 |> sorted_list_of_enum)

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let _ = run_test_tt_main suite
