open Batteries

module type MINSTACK = sig
  exception Empty_Stack
  type 'a t
  val push : 'a t -> 'a -> unit
  val create : unit -> 'a t
  val pop : 'a t -> unit
  val top : 'a t -> 'a
  val get_min : 'a t -> 'a
end

module MinStack : MINSTACK = struct
  type 'a t = { stack : 'a Stack.t
              ; minstack : 'a Stack.t
              ; min : int}

  let push {stack;min;ht} a =
    let min = Int.min min a in
    Stack.push a stack;
    Hashtbl.modify_def 0 a (fun x -> x + 1) ht;
    {stack;min;ht}

  let create () =
    { stack = Stack.create ()
    ; min = Int.max_num
    ; ht = Hashtbl.create 10}

  let pop {stack;min;ht} =
    let a = Stack.pop stack in


end
