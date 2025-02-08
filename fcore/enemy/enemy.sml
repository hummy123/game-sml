structure Enemy =
struct
  open GameType

  (* returns a vector of enemies, with new state (like position, etc.).
   * Also filters any enemies from list if defeated.
   * Called once per frame. *)
  fun updateEnemyList
    ( pos
    , enemies
    , projectiles
    , projectileTree
    , walls
    , wallTree
    , platforms
    , platformTree
    , player
    , graph
    , enemyList
    ) =
    if pos < 0 then
      Vector.fromList enemyList
    else
      let
        val enemy = Vector.sub (enemies, pos)

        (* call function to act on variant, either:
         * 1. updating enemy  and :: cons :: ing to enemyList, or 
         * 2. filtering enemy if projectile hit which enemy should not survive
         * *)
        val enemyList = EnemyBehaviour.updateEnemyState
          ( enemy
          , projectiles
          , projectileTree
          , walls
          , wallTree
          , platforms
          , platformTree
          , player
          , graph
          , enemyList
          )
      in
        updateEnemyList
          ( pos - 1
          , enemies
          , projectiles
          , projectileTree
          , walls
          , wallTree
          , platforms
          , platformTree
          , player
          , graph
          , enemyList
          )
      end

  fun helpGenerateTree (pos, enemyVec: enemy vector, acc) =
    if pos = Vector.length enemyVec then
      acc
    else
      let
        val {id, x, y, ...} = Vector.sub (enemyVec, pos)

        val size = Constants.enemySize

        val acc = QuadTree.insert (x, y, size, size, id, acc)
      in
        helpGenerateTree (pos + 1, enemyVec, acc)
      end

  fun generateTree enemyVec =
    helpGenerateTree
      ( 0
      , enemyVec
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  fun helpFind (findNum, vec: enemy vector, low, high) =
    (* should only be called when we know enemy already exists in vec *)
    let
      val mid = low + ((high - low) div 2)
      val enemy = Vector.sub (vec, mid)
      val {id = curNum, ...} = enemy
    in
      if curNum = findNum then enemy
      else if curNum < findNum then helpFind (findNum, vec, mid + 1, high)
      else helpFind (findNum, vec, low, mid - 1)
    end

  fun find (findNum, vec) =
    helpFind (findNum, vec, 0, Vector.length vec - 1)

  fun helpGetDrawVec (enemy: enemy, width, height) =
    let
      val {x, y, ...} = enemy
      val wratio = width / Constants.worldWidthReal
      val hratio = height / Constants.worldHeightReal
    in
      if wratio < hratio then
        let
          val scale = Constants.worldHeightReal * wratio
          val yOffset =
            if height > scale then (height - scale) / 2.0
            else if height < scale then (scale - height) / 2.0
            else 0.0

          val x = Real32.fromInt x * wratio
          val y = Real32.fromInt y * wratio + yOffset

          val realSize = Constants.enemySizeReal * wratio
        in
          Block.lerp (x, y, realSize, realSize, width, height, 0.5, 0.5, 1.0)
        end
      else
        let
          val scale = Constants.worldWidthReal * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0

          val x = Real32.fromInt x * hratio + xOffset
          val y = Real32.fromInt y * hratio

          val realSize = Constants.enemySizeReal * hratio
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
