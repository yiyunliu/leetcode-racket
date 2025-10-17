let rec subsets xs =
  match xs with
  | [] -> [[]]
  | x::xs ->
     let sub = subsets xs in
     List.append sub (List.map (fun s -> x :: s) sub)


let rec subsets_tr xs =
  let f subsets x =
    List.append (List.map (List.cons x) subsets) subsets in
  List.fold_left f [[]] xs
