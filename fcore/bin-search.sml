structure BinSearch =
struct
  local
    fun helpFind (findNum, vec, low, high) =
      if high >= low then
        let
          val mid = low + ((high - low) div 2)
          val curNum = Vector.sub (vec, mid)
        in
          if curNum = findNum then
            mid
          else if curNum < findNum then
            helpFind (findNum, vec, mid + 1, high)
          else
            helpFind (findNum, vec, low, mid - 1)
        end
      else
        ~1
  in
    fun find (findNum, vec) =
      helpFind (findNum, vec, 0, Vector.length vec - 1)
  end
end
