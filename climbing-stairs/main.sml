structure Lazy = SMLofNJ.Susp;

fun climbStairs (n : int) =
  let val rec cache =
	  Vector.tabulate (n, fn i => Lazy.delay (fn _ => go i))
      fun cacheRef i =
	      Lazy.force (Vector.sub (cache,i))
      fun go 0 = 1
	    | go 1 = 1
	    | go i = cacheRef (i - 1) + cacheRef (i - 2) in
      go n
  end
