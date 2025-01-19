signature BIN_VEC =
sig
  (* char is just 8 bits, which is smaller than int's 32 bits 
   * and smaller = faster here*)
  type elem = char
  type t = char vector

  val singleton: elem -> t
  val getIndex: t * elem -> int
  val insert: t * elem -> t
end

structure BinVec: BIN_VEC =
struct
  type elem = char
  type t = char vector

  fun singleton x = Vector.fromList [x]

  fun helpFind (findNum, vec, low, high) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val curNum = Vector.sub (vec, mid)
      in
        if curNum = findNum then mid
        else if curNum < findNum then helpFind (findNum, vec, mid + 1, high)
        else helpFind (findNum, vec, low, mid - 1)
      end
    else
      ~1

  fun getIndex (vec: t, findNum: elem) =
    helpFind (findNum, vec, 0, Vector.length vec - 1)

  fun reverseLinearSearch (pos, findNum, vec) =
    if pos < 0 then
      ~1
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if findNum > curNum then pos + 1
        else reverseLinearSearch (pos - 1, findNum, vec)
      end

  fun forwardLinearSearch (pos, findNum, vec) =
    if pos = Vector.length vec then
      Vector.length vec
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if findNum > curNum then pos
        else forwardLinearSearch (pos + 1, findNum, vec)
      end

  fun helpFindInsPos (findNum, vec, low, high, prevMid) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val curNum = Vector.sub (vec, mid)
      in
        if curNum = findNum then
          mid
        else if curNum < findNum then
          helpFindInsPos (findNum, vec, mid + 1, high, mid)
        else
          helpFindInsPos (findNum, vec, low, mid - 1, mid)
      end
    else
      let
        val curNum = Vector.sub (vec, prevMid)
      in
        if findNum < curNum then forwardLinearSearch (prevMid, findNum, vec)
        else reverseLinearSearch (prevMid, findNum, vec)
      end

  fun findInsPos (findNum, vec) =
    if Vector.length vec = 0 then ~1
    else helpFindInsPos (findNum, vec, 0, Vector.length vec - 1, 0)

  fun insert (vec: t, elem: elem) =
    let
      val insPos = findInsPos (elem, vec)
    in
      if insPos < 0 then
        Vector.concat [Vector.fromList [elem], vec]
      else if insPos = Vector.length vec then
        Vector.concat [vec, Vector.fromList [elem]]
      else
        let
          val elem = Vector.fromList [elem]
          val elem = VectorSlice.full elem

          val s2len = Vector.length vec - insPos
          val slice1 = VectorSlice.slice (vec, 0, SOME insPos)
          val slice2 = VectorSlice.slice (vec, insPos, SOME s2len)
        in
          VectorSlice.concat [slice1, elem, slice2]
        end
    end

  fun delete (vec, elem) =
    let
      val insPos = findInsPos (elem, vec)
    in
      if insPos < 0 orelse insPos = Vector.length vec then
        vec
      else
        let
          val slice1 = VectorSlice.slice (vec, 0, SOME insPos)

          val slice2Len = Vector.length vec - insPos - 1
          val slice2 = VectorSlice.slice (vec, insPos + 1, SOME slice2Len)
        in
          VectorSlice.concat [slice1, slice2]
        end
    end
end
