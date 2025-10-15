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

let make_test expected k xs =
  assert_equal (BatList.sort compare expected) (List.enum xs |> k_closest k |> sorted_list_of_enum)

let test1 _ = make_test [(2,0);(0,2)] 2 [(0,2);(2,0);(2,2)]
let test2 _ = make_test [(0,2)] 1 [(0,2);(2,2)]
let test3 _ = make_test [(-2,2)] 1 [(1,3);(-2,2)]
let test4 _ = make_test [(3,3);(-2,4)] 2 [(3,3);(5,-1);(-2,4)]

let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2
    ;"test3" >:: test3
    ;"test4" >:: test4]

let _ = run_test_tt_main suite
