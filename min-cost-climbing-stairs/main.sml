structure Lazy = SMLofNJ.Susp

fun minCostClimbingStairs cost =
    let val box = ref NONE
	val len = Vector.length cost
	fun getCost i = Vector.sub(cost,i)
	fun cacheRef i = Lazy.force (Vector.sub (Option.valOf (!box), i))
	fun go 0 = 0
	  | go 1 = 0
	  | go n =
	    let fun mincostn n = cacheRef n + getCost n
	    in Int.min(mincostn (n -1), mincostn (n -2)) end
    in box := SOME (Vector.tabulate
			( len + 1
			, fn i => Lazy.delay (fn _ => (go i))));
       go len
    end
