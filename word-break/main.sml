signature TRIE = sig
  type t
  val makeTrie : unit -> t
  val insert : t * string -> unit
  val isPrefix : t * string -> bool
  val queryChar : t * char -> (bool * t) option
end

structure Trie :> TRIE = struct
  datatype t = Node of bool ref * t option array

  fun char_to_index i = Char.ord i - Char.ord #"a"

  val alphabet_size = 26

  fun makeTrie () : t =
      Node (ref false, Array.array (alphabet_size,NONE))

  fun insert_slice ((trie as Node (is_word, children)), str) =
      case Substring.getc str
       of SOME(ch, str) =>
	  let val ch = char_to_index ch
	      val new_node =
	      case Array.sub (children, ch)
	       of NONE =>
		  let val new_node = makeTrie () in
		      (Array.update (children,ch,SOME new_node); new_node)
		  end
	       | SOME node => node in
	      insert_slice (new_node, str)
	  end
       | NONE => is_word := true

  fun insert (trie,str) = insert_slice(trie,Substring.full str)

  fun queryChar (trie, ch) =
      let val ch = char_to_index ch
	  and Node (_, tries) = trie
      in case Array.sub(tries,ch)
	  of NONE => NONE
	   | SOME (trie as Node (is_word, _)) =>
	     SOME (!is_word, trie)
      end

  fun is_prefix_slice (trie as Node (_, tries), str) =
      case Substring.getc str
       of NONE => true
	| SOME (ch,str) =>
	  case queryChar (trie, ch)
	   of NONE => false
	    | SOME (_, trie) => is_prefix_slice (trie,str)

  fun isPrefix (trie, str) = is_prefix_slice (trie, Substring.full str)
end
