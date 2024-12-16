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

  fun helpGetDrawVec (pos, wallVec, acc, winWidth, winHeight) =
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
        helpGetDrawVec (pos + 1, wallVec, acc, winWidth, winHeight)
      end

  fun getDrawVec (wallVec, width, height) =
    helpGetDrawVec (0, wallVec, [], width, height)
end
