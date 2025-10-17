open Batteries
open OUnit2

type cell = Treasure | Water | Land

let cell_of_int i =
  match i with
  | 0 -> Treasure
  | -1 -> Water
  | _ -> Land

let islands_and_treasure grid =

  let num_rows = Array.length grid
  and num_cols = Array.length (grid.(0)) in
  let discovered = Array.make_matrix num_rows num_cols false in
  let set (x,y) lvl = grid.(x).(y) <- lvl in
  let get (x,y) = grid.(x).(y) in
  let is_discovered (x,y) = discovered.(x).(y) in
  let discover (x,y) = discovered.(x).(y) <- true in
  let get_neighbors (row, col) =
    [(row + 1, col); (row - 1, col); (row, col + 1); (row, col -1)]
    |> List.filter (fun (row,col) ->
           row < num_rows && col < num_cols && 0 <= row && 0 <= col) in
  (* invariant: every node in nodes is lvl distance away from
   the initial node v and is not discovered *)
  let rec bfs v lvl nodes =
    let rec process_nodes nodes new_nodes =
      match nodes with
      | [] -> Some new_nodes
      | node::nodes ->
         discover node;
         let node_ty = cell_of_int (get node) in
         match node_ty with
         | Water -> process_nodes nodes new_nodes
         | Land -> process_nodes nodes
                     (List.append (get_neighbors node |> List.filter (not % is_discovered)) new_nodes)
         | Treasure -> None in
    match process_nodes nodes [] with
    | None -> set v lvl
    | Some new_nodes ->
       bfs v (lvl + 1) new_nodes in
  for i = 0 to num_rows - 1 do
    for j = 0 to num_cols - 1 do
      let node = (i,j) in
      if cell_of_int (get node) = Land then
        begin
          bfs node 0 [node];
          Array.iter
            (fun arr -> Array.fill arr 0 num_cols false)
            discovered
        end
    done
  done

let test1 _ =
  let input =
    [|[|2147483647;-1;0;2147483647|];
      [|2147483647;2147483647;2147483647;-1|];
      [|2147483647;-1;2147483647;-1|];
      [|0;-1;2147483647;2147483647|]|] in
  let output =
    [|[|3;-1;0;1|];
      [|2;2;1;-1|];
      [|1;-1;2;-1|];
      [|0;-1;3;4|]|] in
  assert_equal output (islands_and_treasure input; input)
    ~printer:[%derive.show: int array array]

let test2 _ =
  let input =
    [|[|0;-1|];
      [|2147483647;2147483647|]|] in
  let output =
    [|[|0;-1|];
      [|1;2|]|] in
  assert_equal output (islands_and_treasure input; input)
    ~printer:[%derive.show: int array array]


let suite =
  "suite" >:::
    ["test1" >:: test1
    ;"test2" >:: test2]

let _ = run_test_tt_main suite
