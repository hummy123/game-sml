signature MAKE_BIN_VEC =
sig
  type elem

  val l: elem * elem -> bool
  val eq: elem * elem -> bool
  val g: elem * elem -> bool
end

signature BIN_VEC =
sig
  type elem

  val empty: elem vector

  val sub: elem vector * int -> elem
  val contains: elem * elem vector -> bool

  val findInsPos: elem * elem vector -> int
  val insert: elem vector * elem * int -> elem vector
  val delete: elem vector * elem -> elem vector
end

functor MakeBinVec(Fn: MAKE_BIN_VEC): BIN_VEC =
struct
  type elem = Fn.elem

  val empty = Vector.fromList []

  val sub = Vector.sub

  fun reverseLinearSearch (pos, findNum, vec) =
    if pos < 0 then
      ~1
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if Fn.g (findNum, curNum) then pos + 1
        else reverseLinearSearch (pos - 1, findNum, vec)
      end

  fun forwardLinearSearch (pos, findNum, vec) =
    if pos = Vector.length vec then
      Vector.length vec
    else
      let
        val curNum = Vector.sub (vec, pos)
      in
        if Fn.g (findNum, curNum) then pos
        else forwardLinearSearch (pos + 1, findNum, vec)
      end

  fun helpFindInsPos (findNum, vec, low, high, prevMid) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val curNum = Vector.sub (vec, mid)
      in
        if Fn.eq (curNum, findNum) then
          mid
        else if Fn.l (curNum, findNum) then
          helpFindInsPos (findNum, vec, mid + 1, high, mid)
        else
          helpFindInsPos (findNum, vec, low, mid - 1, mid)
      end
    else
      let
        val curNum = Vector.sub (vec, prevMid)
      in
        if Fn.l (findNum, curNum) then
          forwardLinearSearch (prevMid, findNum, vec)
        else
          reverseLinearSearch (prevMid, findNum, vec)
      end

  fun findInsPos (findNum, vec) =
    if Vector.length vec = 0 then ~1
    else helpFindInsPos (findNum, vec, 0, Vector.length vec - 1, 0)

  fun helpContains (findNum, vec, low, high) =
    if high >= low then
      let
        val mid = low + ((high - low) div 2)
        val curNum = Vector.sub (vec, mid)
      in
        if Fn.eq (curNum, findNum) then
          true
        else if Fn.l (curNum, findNum) then
          helpContains (findNum, vec, mid + 1, high)
        else
          helpContains (findNum, vec, low, mid - 1)
      end
    else
      false

  fun contains (findNum, vec) =
    if Vector.length vec = 0 then false
    else helpContains (findNum, vec, 0, Vector.length vec - 1)

  (* insPos parameter should be the unmodified result of calling findInsPos.
   * The reason the insert function does not call findInsPos directly is so,
   * if two BinVecs are used (one for keys and another for values like a map) 
   * then the insert function can be used for both the key vector and value
   * vector *)
  fun insert (vec, elem, insPos) =
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

  fun delete (vec, elem: elem) =
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

structure IntSet =
  MakeBinVec
    (struct
       type elem = int

       val l = Int.<
       fun eq (a, b) = a = b
       val g = Int.>
     end)

structure ValSet =
  MakeBinVec
    (struct
      type elem = {distance: int, from: char}

      (* l, e and q functions are not actually used in the ValSet
      * because the IntSet is meant to contain keys while the ValSet
      * is meant to contain corresponding values, like in a Map structure.
      * However, it's required by the functor, 
      * and it is actually easy to implement so no issue. *)

      fun l ({distance = a, ...}: elem, {distance = b, ...}: elem) =
        a < b

      fun eq ({distance = a, ...}: elem, {distance = b, ...}: elem) =
        a = b

      fun g ({distance = a, ...}: elem, {distance = b, ...}: elem) =
        a > b
     end)
