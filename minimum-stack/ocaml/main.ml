open Batteries
open OUnit2

module type MINSTACK = sig
  type 'a t
  val push : 'a t -> 'a -> unit
  val create : unit -> 'a t
  val pop : 'a t -> unit
  val top : 'a t -> 'a
  val get_min : 'a t -> 'a
end

module MinStack : MINSTACK = struct
  type 'a t = { stack : 'a Stack.t
              ; minstack : 'a Stack.t}

  let create () =
    let stack = Stack.create () in
    let minstack = Stack.create () in
    { stack; minstack }

  let push {stack;minstack} a =
    Stack.push a stack;
    if Stack.is_empty minstack || a <= Stack.top minstack
    then Stack.push a minstack

  let pop {stack;minstack} =
    let a = Stack.pop stack in
    let min = Stack.top minstack in
    if a = min then ignore (Stack.pop minstack)

  let get_min {minstack;_} =
    Stack.top minstack

  let top {stack;_} = Stack.top stack
end

let test1 _ =
  let ms = MinStack.create () in
  MinStack.push ms 1;
  MinStack.push ms 2;
  MinStack.push ms 0;
  assert_equal 0 (MinStack.get_min ms);
  MinStack.pop ms;
  assert_equal 2 (MinStack.top ms);
  assert_equal 1 (MinStack.get_min ms)

let test2 _ =
  let ms = MinStack.create () in
  MinStack.push ms (-2);
  MinStack.push ms 0;
  MinStack.push ms (-3);
  assert_equal (-3) (MinStack.get_min ms);
  MinStack.pop ms;
  assert_equal 0 (MinStack.top ms);
  assert_equal (-2) (MinStack.get_min ms)

let suite =
  "suite" >::: ["test1">::test1
               ;"test2">::test2]

let _ = run_test_tt_main suite
