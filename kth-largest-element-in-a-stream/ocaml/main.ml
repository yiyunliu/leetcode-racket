open Batteries
open OUnit2

module type S = sig
  type t
  val init : int -> int List.t -> t
  val insert : int -> t -> int * t
end

module KthLargest : S = struct
  type t = {size : int; minheap : int Heap.t}
  let empty k = {size = k; minheap = Heap.empty}
  let insert0 i ({size;minheap} as t) =
    if Heap.size minheap < size then
      {t with minheap = Heap.add i minheap}
    else
      let min = Heap.find_min minheap in
      if i < min then t else {t with minheap = Heap.del_min minheap |> Heap.add i }
  let init k = List.fold (fun t x -> insert0 x t) (empty k)
  let insert i t =
    let ({minheap;_} as t) = insert0 i t in
    (Heap.find_min minheap, t)
end


let tests = "test suite for kthlargest"  >::: [
      "test0" >:: (fun _ -> let open KthLargest in
                            let t = init 3 [1;2;3;3] in
                            let (i0,t0) = insert 3 t in
                            let (i1,t1) = insert 5 t0 in
                            let (i2,t2) = insert 6 t1 in
                            let (i3,t3) = insert 7 t2 in
                            let (i4,_) = insert 8 t3 in
                            assert_equal 3 i0;
                            assert_equal 3 i1;
                            assert_equal 3 i2;
                            assert_equal 5 i3;
                            assert_equal 6 i4)
    ]

let _ = run_test_tt_main tests
