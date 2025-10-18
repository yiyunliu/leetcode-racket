open OUnit2

type 'a tree = Empty | Node of 'a node
and 'a node = {left : 'a tree; right : 'a tree; value : 'a}

let rec max_depth = function
  | Empty -> 0
  | Node node -> max_depth_node node
and max_depth_node = function
  | {left;right;_} ->
      Int.max (max_depth left) (max_depth right) + 1

let singleton a = Node {left = Empty; right = Empty; value = a}

let test1 _ =
  assert_equal 3 (Node {value=1;left=singleton 2; right = (Node {left = singleton 4; right = Empty; value = 3})} |> max_depth)

let suite =
  "suite" >:::
    ["test1" >:: test1]

let _ = run_test_tt_main suite
