structure Enemy =
struct
  val size = 35
  val realSize = 35.0

  (* called when filtering enemies,
   * to adjust enemy data on collision with projectile *)
  fun onCollisionWithProjectile (enemy, projectileTree, acc) =
    let
      val {x, y, health, id} = enemy
      val hasCollision = QuadTree.hasCollisionAt
        (x, y, size, size, 0, 0, 1920, 1080, ~1, projectileTree)
    in
      if hasCollision then
        if health = 1 then
          (* filter out if decrementing health by one = 0 *)
          acc
        else
          {health = health - 1, x = x, y = y, id = id} :: acc
      else
        enemy :: acc
    end

  fun helpGenerateTree (pos, enemyVec, acc) =
    if pos = Vector.length enemyVec then
      acc
    else
      let
        val {id, x, y, health = _} = Vector.sub (enemyVec, pos)
        val acc = QuadTree.insert (x, y, size, size, 0, 0, 1920, 1080, id, acc)
      in
        helpGenerateTree (pos + 1, enemyVec, acc)
      end

  fun generateTree enemyVec = helpGenerateTree (0, enemyVec, QuadTree.empty)

  fun helpFind (findNum, vec, low, high) =
    (* should only be called when we know enemy already exists in vec *)
    let
      val mid = low + ((high - low) div 2)
      val enemy = Vector.sub (vec, mid)
      val {id = curNum, x = _, y = _, health = _} = enemy
    in
      if curNum = findNum then enemy
      else if curNum < findNum then helpFind (findNum, vec, mid + 1, high)
      else helpFind (findNum, vec, low, mid - 1)
    end

  fun find (findNum, vec) =
    helpFind (findNum, vec, 0, Vector.length vec - 1)

  fun helpGetDrawVec ({x, y, id = _, health = _}, width, height) =
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

          val x = Real32.fromInt x * wratio
          val y = Real32.fromInt y * wratio + yOffset

          val realSize = realSize * wratio
        in
          Block.lerp (x, y, realSize, realSize, width, height, 0.5, 0.5, 1.0)
        end
      else
        let
          val scale = 1920.0 * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0

          val x = Real32.fromInt x * hratio + xOffset
          val y = Real32.fromInt y * hratio

          val realSize = realSize * hratio
        in
          Block.lerp (x, y, realSize, realSize, width, height, 0.5, 0.5, 1.0)
        end
    end

  fun getDrawVecLoop (pos, enemies, width, height, acc) =
    if pos = Vector.length enemies then
      Vector.concat acc
    else
      let
        val e = Vector.sub (enemies, pos)
        val hd = helpGetDrawVec (e, width, height)
        val acc = hd :: acc
      in
        getDrawVecLoop (pos + 1, enemies, width, height, acc)
      end

  fun getDrawVec (enemies, width, height) =
    getDrawVecLoop (0, enemies, width, height, [])
end
