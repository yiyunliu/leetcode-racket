structure Lazy = SMLofNJ.Susp;

fun climbStairs (n : int) =
  let val boxed = ref NONE
      fun cacheRef i =
	      Lazy.force (Vector.sub (Option.valOf (!boxed),i))
      fun go 0 = 1
	    | go 1 = 1
	    | go i = cacheRef (i - 1) + cacheRef (i - 2)
  in boxed := SOME (Vector.tabulate (n + 1, fn i => Lazy.delay (fn _ => go i)));
     go n
  end
