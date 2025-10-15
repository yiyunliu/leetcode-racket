open Batteries

module type S = sig
  val num_islands : char array array -> int
end

module type GRAPH = sig
  type t
  type vertex
  val neighbors : vertex -> t -> vertex Enum.t
  val vertices : t -> vertex Enum.t
end

module type COMPONENTS =
  functor (G: GRAPH) ->
  sig
    val num_components : G.t -> int
  end

module MatrixGraph : GRAPH with type t = char array array = struct
  type vertex = int * int
  type t = char array array
  let vertices g =
    let num_rows = Array.length g
    and num_cols = Array.length (g.(0)) in
    Enum.unfold 0 (fun x ->
        if x < num_rows then
          Some ((Enum.unfold 0 (fun y ->
                    if y < num_cols then
                      Some ((x, y) ,y + 1)
                    else None)), x + 1)
        else None)
    |> Enum.flatten
  let neighbors (row,col) (g : t) =
    let num_rows = Array.length g
    and num_cols = Array.length (g.(0)) in
    let vertices = [(row,col+1);(row,col-1);(row+1,col);(row-1,col)] in
    List.filter (fun (x,y) -> x >= 0 && x < num_rows && y >= 0 && y < num_cols && g.(x).(y) == '1') vertices
    |> List.enum
end

module Components : COMPONENTS =
  functor (G : GRAPH) -> struct
  open G
  type state =
    {discovered : vertex Set.t; processed : vertex Set.t}

  let init_state = {discovered = Set.empty; processed = Set.empty}

  (* invariant: v is not discovered yet *)
  let rec dfs graph v state =
    let state = {state with discovered = Set.add v state.discovered} in
    let state =
      G.neighbors v graph
      |> Enum.fold (fun state v ->
            if Set.mem v state.discovered
            then state
            else dfs graph v state)
           state in
    {state with processed = Set.add v state.processed}

  let num_components g =
    G.vertices g
    |> Enum.fold (fun (state,acc) v ->
           if Set.mem v state.discovered
           then (state,acc)
           else (dfs g v state, acc+1))
         (init_state, 0)
    |> Tuple2.second
end

module Impl : S = struct
  module IslandComponents = Components(MatrixGraph)
  open IslandComponents
  let num_islands = num_components
end
