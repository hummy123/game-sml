structure Platform =
struct
  (* collision height of a platform.
   * Visual height may (and probably will) be different. *)
  val platHeight = 3
  val rPlatHeight = 3.0

  fun helpGenerateTree (pos, platVec, acc) =
    if pos = Vector.length platVec then
      acc
    else
      let
        val {id, x, y, width} = Vector.sub (platVec, pos)
        val acc = QuadTree.insert
          (x, y, width, platHeight, 0, 0, 1920, 1080, id, acc)
      in
        helpGenerateTree (pos + 1, platVec, acc)
      end

  fun generateTree platVec = helpGenerateTree (0, platVec, QuadTree.empty)

  fun helpFind (findNum, vec, low, high) =
    let
      val mid = low + ((high - low) div 2)
      val platform = Vector.sub (vec, mid)
      val {id = curNum, x = _, y = _, width = _} = platform
    in
      if curNum = findNum then platform
      else if curNum < findNum then helpFind (findNum, vec, mid + 1, high)
      else helpFind (findNum, vec, low, mid - 1)
    end

  fun find (findNum, vec) =
    helpFind (findNum, vec, 0, Vector.length vec - 1)

  fun helpGetDrawVecWider
    (pos, platVec, acc, winWidth, winHeight, ratio, yOffset) =
    if pos = Vector.length platVec then
      Vector.concat acc
    else
      let
        val plat = Vector.sub (platVec, pos)
        val {x, y, width, id = _} = plat

        val x = Real32.fromInt x * ratio
        val y = Real32.fromInt y * ratio + yOffset

        val width = Real32.fromInt width * ratio
        val height = rPlatHeight * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
        val acc = block :: acc
      in
        helpGetDrawVecWider
          (pos + 1, platVec, acc, winWidth, winHeight, ratio, yOffset)
      end

  fun helpGetDrawVecTaller
    (pos, platVec, acc, winWidth, winHeight, ratio, xOffset) =
    if pos = Vector.length platVec then
      Vector.concat acc
    else
      let
        val plat = Vector.sub (platVec, pos)
        val {x, y, width, id = _} = plat

        val x = Real32.fromInt x * ratio + xOffset
        val y = Real32.fromInt y * ratio

        val width = Real32.fromInt width * ratio
        val height = rPlatHeight * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
        val acc = block :: acc
      in
        helpGetDrawVecTaller
          (pos + 1, platVec, acc, winWidth, winHeight, ratio, xOffset)
      end

  fun getDrawVec (platVec, width, height) =
    let
      val wratio = width / 1920.0
      val hratio = height / 1080.0
    in
      if wratio < hratio then
        let
          val scale = 1080.0 * wratio
          val yOffset =
            if height > scale then (height - scale) / 2.0
            else if height < scale then (scale - height) / 2.0
            else 0.0
        in
          helpGetDrawVecWider (0, platVec, [], width, height, wratio, yOffset)
        end
      else
        let
          val scale = 1920.0 * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0
        in
          helpGetDrawVecTaller (0, platVec, [], width, height, hratio, xOffset)
        end
    end
end
