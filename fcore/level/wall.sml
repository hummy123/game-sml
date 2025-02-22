structure Wall =
struct
  type t = {id: int, x: int, y: int, width: int, height: int}

  fun helpGenerateTree (pos, wallVec, acc) =
    if pos = Vector.length wallVec then
      acc
    else
      let
        val {id, x, y, width, height} = Vector.sub (wallVec, pos)
        val acc = QuadTree.insert (x, y, width, height, id, acc)
      in
        helpGenerateTree (pos + 1, wallVec, acc)
      end

  fun generateTree wallVec =
    helpGenerateTree
      ( 0
      , wallVec
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  fun helpGetDrawVecWider
    (pos, wallVec, acc, winWidth, winHeight, ratio, yOffset) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, id = _} = wall

        val x = Real32.fromInt x * ratio
        val y = Real32.fromInt y * ratio + yOffset

        val width = Real32.fromInt width * ratio
        val height = Real32.fromInt height * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 1.0, 1.0, 1.0)
        val acc = block :: acc
      in
        helpGetDrawVecWider
          (pos + 1, wallVec, acc, winWidth, winHeight, ratio, yOffset)
      end

  fun helpGetDrawVecTaller
    (pos, wallVec, acc, winWidth, winHeight, ratio, xOffset) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, id = _} = wall

        val x = Real32.fromInt x * ratio + xOffset
        val y = Real32.fromInt y * ratio

        val width = Real32.fromInt width * ratio
        val height = Real32.fromInt height * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 1.0, 1.0, 1.0)
        val acc = block :: acc
      in
        helpGetDrawVecTaller
          (pos + 1, wallVec, acc, winWidth, winHeight, ratio, xOffset)
      end

  fun getDrawVec (wallVec, width, height) =
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
          helpGetDrawVecWider (0, wallVec, [], width, height, wratio, yOffset)
        end
      else
        let
          val scale = 1920.0 * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0
        in
          helpGetDrawVecTaller (0, wallVec, [], width, height, hratio, xOffset)
        end
    end
end
