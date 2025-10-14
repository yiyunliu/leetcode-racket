signature LAST_STONE_WEIGHT = sig
  val lastStoneWeight : int list -> int
end

structure LastStoneWeight :> LAST_STONE_WEIGHT = struct
  structure IntPriority = struct
    type priority = int
    type item = int
    val compare = Int.compare
    fun priority x = x
  end

  structure Q :> MONO_PRIORITYQ where type item = int =
    LeftPriorityQFn(IntPriority)

  val init = List.foldl Q.insert Q.empty
  fun step (q : Q.queue) =
    let val (i,q) = Q.remove q
	val (j,q) = Q.remove q
    in case Int.compare (i,j) of
	 LESS => Q.insert (j-i,q)
       | EQUAL => q
       | GREATER => Q.insert (i-j,q)
    end

  fun loop (q : Q.queue) =
      let val numItems = Q.numItems q in
	  if numItems = 0
	  then 0
	  else if numItems = 1
	  then let val (i,_) = Q.remove q in i end
	  else loop (step q)
      end

  fun lastStoneWeight stones = loop (init stones)
end


structure Test = struct
  open LastStoneWeight

  val testCases =
    [ ([2,3,6,2,4], 1)
    , ([1,2], 1) ]

  val results =
      List.map
	(fn (stones,expected) =>
	    ( stones
	    , expected
	    , lastStoneWeight stones = expected))
	testCases
end
