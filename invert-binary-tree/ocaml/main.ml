open Batteries

type 'a tree = Empty | Node of {value : 'a; mutable left : 'a tree; mutable right : 'a tree}

let rec invert_tree t =
  match t with
  | Empty -> Empty
  | Node {value;left;right} ->
     let right = invert_tree left
     and left = invert_tree right in
     Node {value;left;right}
