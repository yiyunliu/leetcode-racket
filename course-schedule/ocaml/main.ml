open OUnit2

module type SCHEDULE = sig
  val can_finish : int -> (int * int) list -> bool
end

module Schedule : SCHEDULE = struct
  type graph = int list array
  let get_neighbors i (g : graph) = g.(i)

  let insert_edge x y g =
    g.(x) <- y :: g.(x)

  let create k : graph = Array.make k []

  let is_cyclic graph =
    let k = Array.length graph in
    let processed = Array.make k false in
    let discovered = Array.make k false in
    (* invariant: v is in the discovered set, but not the processed set  *)
    let rec dfs v =
      let neighbors = get_neighbors v graph in
      let rec process_neighbors nodes =
        match nodes with
        | [] -> processed.(v) <- true; false
        | node::nodes ->
           if not (discovered.(node)) then
             begin
               discovered.(node) <- true;
               dfs node || process_neighbors nodes
             end
           else if processed.(node) then
             process_neighbors nodes
           else true in
      process_neighbors neighbors in
    let rec loop node =
      if node < k then
        if processed.(node) then
          loop (node + 1)
        else dfs node || loop (node + 1)
      else false in
    loop 0

  let can_finish k edges =
    let graph = create k in
    edges |> List.iter (fun (tgt,src) -> insert_edge src tgt graph);
    is_cyclic graph |> not
end

open Schedule

let test1 _ = assert_equal true (can_finish 2 [(0,1)]) ~printer:Bool.to_string
let test2 _ = assert_equal false (can_finish 2 [(0,1);(1,0)]) ~printer:Bool.to_string
let test3 _ = assert_equal true (can_finish 4 [(1,0);(2,0);(3,1);(3,2)]) ~printer:Bool.to_string
let test4 _ = assert_equal false (can_finish 4 [(1,0);(2,0);(3,1);(3,2);(0,3)]) ~printer:Bool.to_string

let suite = "suite" >::: ["test1" >:: test1; "test2" >:: test2; "test3" >:: test3; "test4" >:: test4]
let _ = run_test_tt_main suite
