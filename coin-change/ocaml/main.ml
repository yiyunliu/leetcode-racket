open OUnit2

let min_int_opt a b =
  match a , b with
  | None, b -> b
  | a, None -> a
  | Some a, Some b -> Some (Int.min a b)

let coin_change (coins : int array) (amount : int) : int =
  let rec cache = lazy(Array.init amount
                         (fun amount -> lazy (go amount)))
  and cache_ref i = Lazy.force ((Lazy.force cache).(i))
  and go i = match i with
    | 0 -> Some 0
    | i ->
       BatArray.enum coins
       |> BatEnum.map ((-) i)
       |> BatEnum.filter ((<=) 0)
       |> BatEnum.map cache_ref
       |> BatEnum.fold min_int_opt None
       |> Option.map ((+)1) in
  match go amount with
  | None -> ~-1
  | Some r -> r

let test1 _ =
  assert_equal 3 (coin_change [|1;5;10|] 12)
    ~printer:string_of_int

let test2 _ =
  assert_equal ~-1 (coin_change [|2|] 3)
    ~printer:string_of_int

let test3 _ =
  assert_equal 2 (coin_change [|1;5;10|] 20)
    ~printer:string_of_int

let suite =
  "suite" >:::
    ["test1">::test1
    ;"test2">::test2
    ;"test3">::test3]

let _ = run_test_tt_main suite
