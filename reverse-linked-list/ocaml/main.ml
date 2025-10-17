open Batteries
open OUnit2

type listnode = {value : int; mutable next : mylist}
and mylist = listnode option


let mylist_rev xs =
  let rec loop prev xs =
    match xs with
    | None -> ()
    | Some ({next;_} as node) ->
       node.next <- prev;
       loop (Some node) next in
  loop None xs
