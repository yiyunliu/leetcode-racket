open Batteries
open OUnit2

module type S = sig
  val schedule_tasks : char Enum.t -> int -> int
end

module TaskOrd = struct
  type t = int * char
  let compare (x,_) (y,_) = compare x y
end

module TaskHeap = Heap.Make(TaskOrd)

module I : S = struct
  let schedule_tasks tasks n =
    let f (heap,count) task =
      let count = Map.modify_def 0 task (fun x -> x + 1) count in
      let prio = Map.find task count in
      let heap = TaskHeap.add (prio,task) heap in
      (heap, count) in
    let (heap,_) = Enum.fold f (TaskHeap.empty,Map.empty) tasks in
    let f (prev,acc) (_,task) =
      if prev = Some task then
        let acc = acc + n in
        (prev,acc)
      else let prev = Some task in
           let acc = acc + 1 in
           (prev,acc) in
    heap |> TaskHeap.enum |> Enum.fold f (None,0) |> Tuple2.second
end

let make_test expected tasks n =
  fun _ ->
  assert_equal expected (I.schedule_tasks (List.enum tasks) n)
    ~printer:string_of_int

let test1 = make_test 5 ['X';'X';'Y';'Y'] 2
let test2 = make_test 9 ['A';'A';'A';'B';'C'] 3

let suite = "suite" >::: ["test1">::test1;"test2">::test2]
let _ = run_test_tt_main suite
