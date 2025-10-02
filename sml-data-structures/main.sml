structure StringTable :> MONO_HASH_TABLE
  where type Key.hash_key = string =
  HashTableFn (struct
		type hash_key = string
		val hashVal = HashString.hashString
		fun sameKey (x,y) = String.compare (x,y) = EQUAL
	      end);

structure StringMap :> ORD_MAP where type Key.ord_key = string
  = RedBlackMapFn (struct type ord_key = string
			  val compare = String.compare end)
