structure Lazy = SMLofNJ.Susp

fun rob nums =
    let val slice0 = VectorSlice.slice (nums,1,NONE)
	val slice1 = VectorSlice.slice (nums,0,SOME (Vector.length nums - 1))
    in Int.max (robseq slice0, robseq slice1) end

fun robseq nums =
    let val len = VectorSlice.length nums
	val box = ref NONE
	fun cacheRef i = Lazy.force(Vector.sub(Option.valOf (!box),i))
	fun getMoney i = VectorSlice.sub(nums,i)
	fun go 0 = getMoney 0
	  | go 1 = Int.max (cacheRef 0, getMoney 1)
	  | go n = Int.max(cacheRef (n - 2) + getMoney n, cacheRef (n - 1))
    in box := SOME (Vector.tabulate (len, fn i => Lazy.delay (fn _ => go i)));
       go (len - 1) end


structure Test = struct
val testCases = #[(#[1,1,3,3],4)
		 ,(#[2,9,8,3,6],15)
		 ,(#[1,1,1],1)]
val testResults = Vector.map
		      (fn (nums,expected) => rob nums = expected)
		      testCases
end
