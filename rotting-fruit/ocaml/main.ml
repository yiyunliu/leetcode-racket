open OUnit2

module type S = sig
  val orange_rotting : int array array -> int
end

module Impl : S = struct
  type node_type = Banana | Empty | Rotten

  let node_type_of_int i =
    match i with
    | 0 -> Empty
    | 1 -> Banana
    | 2 -> Rotten
    | _ -> raise (Failure "impossible")

  let orange_rotting matrix =
    let num_rows = Array.length matrix in
    let num_cols = Array.length (matrix.(0)) in

    (* also functions as the discovered matrix *)
    let dist = Array.make_matrix num_rows num_cols None in
    let get_dist (x,y) = Option.get (dist.(x).(y)) in
    let can_add (x,y) =
      x < num_rows && x >= 0 && y < num_cols && y >= 0 &&
        (node_type_of_int (matrix.(x).(y)) = Banana) && (dist.(x).(y) = None) in
    let get_undiscovered_neighbors (x,y) =
      List.filter (fun n -> can_add n) [(x+1,y);(x-1,y);(x,y+1);(x,y-1)] in
    let queue = BatQueue.create  () in
    for i = 0 to num_rows - 1 do
      for j = 0 to num_cols - 1 do
        match node_type_of_int (matrix.(i).(j)) with
        | Rotten ->
           BatQueue.add (i,j) queue;
           dist.(i).(j) <- Some 0
        | _ -> ()
      done
    done;
    let rec bfs () =
      if not (BatQueue.is_empty queue) then
        let node = BatQueue.pop queue in
        let d = get_dist node in
        get_undiscovered_neighbors node
        |> List.iter
             begin
               fun ((x,y) as node) ->
               dist.(x).(y) <- Some (d + 1);
               BatQueue.add node queue
             end;
        bfs () in
    bfs ();
    let iter = BatEnum.range 0 ~until:(num_rows - 1)
               |> BatEnum.concat_map
                    (fun x ->
                      BatEnum.range 0 ~until:(num_cols - 1)
                      |> BatEnum.map (fun y -> (x,y))
                      |> BatEnum.filter (fun (x,y) ->
                             node_type_of_int (matrix.(x).(y)) = Banana)) in
    let rec loop acc =
      match BatEnum.get iter with
      | None -> acc
      | Some (x,y) ->
         match dist.(x).(y) with
         | Some d -> loop (max acc d)
         | None -> -1 in
    loop 0
end


let test1 _ =
  assert_equal 4 (Impl.orange_rotting [| [|1;1;0|];[|0;1;1|];[|0;1;2|] |]) ~printer:string_of_int

let test2 _ =
  assert_equal (-1) (Impl.orange_rotting [| [|1;0;0|];[|0;1;1|];[|0;1;2|] |]) ~printer:string_of_int

let test3 _ =
  assert_equal (-1) (Impl.orange_rotting [| [|1;0;1|];[|0;2;0|];[|1;0;1|] |]) ~printer:string_of_int

let suite = "suite" >::: ["test1" >:: test1; "test2" >:: test2; "test3" >:: test3]

let _ = run_test_tt_main suite
