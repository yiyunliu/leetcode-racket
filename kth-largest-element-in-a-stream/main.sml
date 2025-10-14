local structure IntPriority = struct
      type priority = int
      type item = int
      fun compare (x,y) = Int.compare (y,x)
      fun priority x = x
end in
structure IntQueue :> MONO_PRIORITYQ where type item = int =
	  LeftPriorityQFn(IntPriority)
end


signature KTHLARGEST = sig
  type t
  val empty : int -> t
  val fromList : int * int list -> t
  val insert : int * t -> t
  val kthLargest : t -> int
end

structure KthLargest :> KTHLARGEST = struct
  structure Q = IntQueue
  type t = {heap : Q.queue, size : int}

  fun updateHeap (newHeap,{heap,size}) = {heap = newHeap, size = size}
  fun empty k = {heap = Q.empty, size = k}
  fun insert (a,r as {heap, size}) =
      if Q.numItems heap < size
      then updateHeap (Q.insert (a,heap), r)
      else let val (n, heap) = Q.remove heap
	   in if a < n
	      then r
	      else updateHeap (Q.insert (a,heap), r)
	   end
  fun fromList (k,xs) =
      List.foldl insert (empty k) xs
  fun kthLargest ({heap,...} : t) =
      let val (i,_) = Q.remove heap in i end
end

structure TEST = struct
  open KthLargest
  val testResults : bool list =
      let val r = fromList (3,[1,2,3,3])
	  val r0 = insert (3,r)
	  val r1 = insert (5,r0)
	  val r2 = insert (6,r1)
	  val r3 = insert (7,r2)
	  val r4 = insert (8,r3) in
      [ kthLargest r = 2
      , kthLargest r0 = 3
      , kthLargest r1 = 3
      , kthLargest r2 = 3
      , kthLargest r3 = 5
      , kthLargest r4 = 6]
      end
end
