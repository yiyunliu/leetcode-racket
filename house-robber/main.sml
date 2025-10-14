structure Lazy = SMLofNJ.Susp
fun rob nums =
    let val len = Vector.length nums
	val box = ref NONE
	fun cacheRef i = Lazy.force(Vector.sub(Option.valOf (!box),i))
	fun getMoney i = Vector.sub(nums,i)
	fun go 0 = getMoney 0
	  | go 1 = getMoney 1
	  | go n = Int.max(cacheRef (n - 2) + getMoney n, cacheRef (n - 1))
    in box := SOME (Vector.tabulate (len, fn i => Lazy.delay (fn _ => go i)));
       go (len - 1) end


structure Test = struct
val testCases = #[(#[1,1,3,3],4)
		 ,(#[2,9,8,3,6],16)
		 ,(#[1,2,3,1],4)
		 ,(#[2,7,9,3,1],12)
		 ,(#[1,1,1],2)]
val testResults = Vector.map
		      (fn (nums,expected) => rob nums = expected)
		      testCases
end
