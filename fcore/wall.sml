structure Wall =
struct
  type t = {id: int, x: int, y: int, width: int, height: int}

  val wall1 = {id = 1, x = 0, y = 0, width = 100, height = 1080}
  val wall2 = {id = 2, x = 1820, y = 0, width = 100, height = 1080}
  val wall3 = {id = 3, x = 0, y = 980, width = 1920, height = 108}
  val wall4 = {id = 4, x = 155, y = 911, width = 155, height = 55}

  val wallVec = Vector.fromList [wall1, wall2, wall3, wall4]

  fun getID n =
    Vector.sub (wallVec, n - 1)

  fun generateTree (pos, wallVec, acc) =
    if pos = Vector.length wallVec then
      acc
    else
      let
        val {id, x, y, width, height} = Vector.sub (wallVec, pos)
        val acc = QuadTree.insert
          (x, y, width, height, 0, 0, 1920, 1080, id, acc)
      in
        generateTree (pos + 1, wallVec, acc)
      end

  val tree =
    let
      val {id, x, y, width, height} = Vector.sub (wallVec, 0)
      val acc = QuadTree.fromItem (id, x, y, width, height)
    in
      generateTree (1, wallVec, acc)
    end

  fun helpGenerateWalls (pos, wallVec, acc, winWidth, winHeight) =
    if pos = Vector.length wallVec then
      Vector.concat acc
    else
      let
        val wall = Vector.sub (wallVec, pos)
        val {x, y, width, height, ...} = wall
        val width = Real32.fromInt width
        val height = Real32.fromInt height
        val block = Block.lerp
          (x, y, width, height, winWidth, winHeight, 0.0, 0.0, 0.0)
        val acc = block :: acc
      in
        helpGenerateWalls (pos + 1, wallVec, acc, winWidth, winHeight)
      end

  fun generateWalls () =
    helpGenerateWalls (0, wallVec, [], 1920.0, 1080.0)
end
