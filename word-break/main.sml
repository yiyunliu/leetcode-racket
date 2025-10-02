signature WORDBREAK = sig
  val wordbreak : string list -> string -> bool
end

structure WordBreak :> WORDBREAK = struct
  fun process_char itrie (ch, (acc,_))
      = let val (tries, is_word) =
	    List.foldl
		(fn (trie,acc as (acc_ls,acc_bool)) =>
		    case Trie.queryChar (trie, ch)
		     of NONE => acc
		      | SOME (is_word, trie) =>
			(trie :: acc_ls, is_word orelse acc_bool))
		([],false) acc
	in (if is_word then (itrie :: tries) else tries, is_word)
	end

  fun wordbreak words =
      let val trie = Trie.makeTrie () in
	  List.app (fn word => Trie.insert (trie,word)) words;
	  fn str =>
	     #2 (CharVector.foldl
		     (process_char trie)
		     ([trie],false) str)
      end
end


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
		      (Array.update (children,ch,SOME new_node);
		       new_node)
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
