open OUnit2

module type TRIE = sig
  type t
  val insert_string : string -> t -> unit
  val mem : string -> t -> bool
  val has_as_prefix : string -> t -> bool
  val create : unit -> t
end

module Trie : TRIE = struct
  type t = { mutable is_word : bool; children : t option array }

  let index_of_char =
    let a_idx = Char.code 'a' in
    fun x -> Char.code x - a_idx

  let create () =
    {is_word = false;children = Array.make 26 None}

  let insert_char {children;_} ch =
    let ch = index_of_char ch in
    match children.(ch) with
    | None ->
       let new_node = create () in
       children.(ch) <- Some new_node;
       new_node
    | Some node ->
       node

  let insert_string str node =
    let child = String.fold_left insert_char node str in
    child.is_word <- true

  let mem str node =
    let len = String.length str in
    let rec loop node i =
      if i >= len then node.is_word
      else let ch = index_of_char (str.[i]) in
           match node.children.(ch) with
           | None -> false
           | Some node -> loop node (i + 1) in
    loop node 0

  let has_as_prefix str node =
    let len = String.length str in
    let rec loop node i =
      i >= len ||
        let ch = index_of_char (str.[i]) in
        match node.children.(ch) with
        | None -> false
        | Some node -> loop node (i + 1) in
    loop node 0
end

let test1 _ =
  let trie = Trie.create () in
  ["dog";"dog";"do";"back";"hella";"hello";"hej"]
  |> List.iter (fun word -> Trie.insert_string word trie);
  assert_equal true (Trie.mem "dog" trie);
  assert_equal true (Trie.mem "do" trie);
  assert_equal false (Trie.mem "d" trie);
  assert_equal true (Trie.has_as_prefix "d" trie);
  assert_equal true (Trie.has_as_prefix "he" trie);
  assert_equal true (Trie.has_as_prefix "hel" trie);
  assert_equal false (Trie.has_as_prefix "q" trie);
  assert_equal false (Trie.has_as_prefix "hed" trie)

let suite = "suite" >::: ["test1" >:: test1]
let _ = run_test_tt_main suite
