structure Wall =
struct
  fun helpGenerateTree (pos, wallVec, acc) =
    if pos = Vector.length wallVec then
      acc
    else
      let
        val {id, x, y, width, height} = Vector.sub (wallVec, pos)
        val acc = QuadTree.insert
          (x, y, width, height, 0, 0, 1920, 1080, id, acc)
      in
        helpGenerateTree (pos + 1, wallVec, acc)
      end

  fun generateTree wallVec = helpGenerateTree (0, wallVec, QuadTree.empty)

  val preferredAspectRatio: Real32.real = 1920.0 / 1080.0

  fun helpGetDrawVecPreferred (pos, wallVec, acc, winWidth, winHeight) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, id = _} = wall
        val width = Real32.fromInt width
        val height = Real32.fromInt height

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
        val acc = block :: acc
      in
        helpGetDrawVecPreferred (pos + 1, wallVec, acc, winWidth, winHeight)
      end

  (* when actual aspect ratio > preferredAspectRatio *)
  fun helpGetDrawVecWider
    (pos, wallVec, acc, winWidth, winHeight, ratio, yOffset) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, id = _} = wall

        val x = Real32.fromInt x * ratio
        val x = Real32.toInt IEEEReal.TO_NEAREST x

        val y = Real32.fromInt y * ratio + yOffset
        val y = Real32.toInt IEEEReal.TO_NEAREST y

        val width = Real32.fromInt width * ratio
        val height = Real32.fromInt height * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
        val acc = block :: acc
      in
        helpGetDrawVecWider
          (pos + 1, wallVec, acc, winWidth, winHeight, ratio, yOffset)
      end

  (* when actual aspect ratio < preferredAspectRatio *)
  fun helpGetDrawVecTaller
    (pos, wallVec, acc, winWidth, winHeight, ratio, xOffset) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, id = _} = wall

        val x = Real32.fromInt x * ratio + xOffset
        val x = Real32.toInt IEEEReal.TO_NEAREST x

        val y = Real32.fromInt y * ratio
        val y = Real32.toInt IEEEReal.TO_NEAREST y

        val width = Real32.fromInt width * ratio
        val height = Real32.fromInt height * ratio

        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
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
