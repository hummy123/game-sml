structure Enemy =
struct
  fun helpExists (pos, id, collisions) =
    if pos = Vector.length collisions then
      false
    else
      let val current = Vector.sub (collisions, pos)
      in current = id orelse helpExists (pos + 1, id, collisions)
      end

  fun exists (id, collisions) = helpExists (0, id, collisions)

  fun getPatrollPatches (enemy, wallTree, platformTree, acc) =
    let
      (* This function is meant to check 
       * if enemy should switch the horizontal direction 
       * if the enemy is patrolling.
       *
       * Algorithm:
       * 1. Check if enemy there is a wall ahead of the enemy
       *    in the direction the enemy is walking.
       * 1.1. If there is a wall, then invert the direction.
       *
       * 2. If there is no wall, check if there is space to 
       *    walk ahead on, such that enemy will not fall
       *    if enemy continues to walk.
       * 2.1. If continuing to walk will cause the enemy to fall,
       *      then invert the direction.
       *
       * 3. Else, do not invert direction and simply return given list.
       * *)
    in

    end

  (* called when filtering enemies,
   * to adjust enemy data on collision with projectile *)
  fun onCollisionWithProjectile
    (enemy, projectileTree, acc, walls, wallTree, platforms, platformTree) =
    let
      val {x, y, health, id, xAxis, yAxis} = enemy

      val size = Constants.enemySize
      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val hasCollision = QuadTree.hasCollisionAt
        (x, y, size, size, 0, 0, ww, wh, ~1, projectileTree)
    in
      if hasCollision then
        if health = 1 then
          (* filter out if decrementing health by one = 0 *)
          acc
        else
          let
            val patches = EnemyPhysics.getPhysicsPatches enemy
            val patches = EnemyPatch.W_HEALTH (health - 1) :: patches
            val enemy = EnemyPatch.withPatches (enemy, patches)

            val patches = EnemyPhysics.getEnvironmentPatches
              (enemy, walls, wallTree, platforms, platformTree)
            val enemy = EnemyPatch.withPatches (enemy, patches)
          in
            enemy :: acc
          end
      else
        let
          val patches = EnemyPhysics.getPhysicsPatches enemy
          val enemy = EnemyPatch.withPatches (enemy, patches)

          val patches = EnemyPhysics.getEnvironmentPatches
            (enemy, walls, wallTree, platforms, platformTree)
          val enemy = EnemyPatch.withPatches (enemy, patches)
        in
          enemy :: acc
        end
    end

  (* filter enemy projectiles when player is not attacking *)
  fun filterProjectileCollisions
    ( pos
    , enemies
    , projectileTree
    , acc
    , walls
    , wallTree
    , platforms
    , platformTree
    ) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)
        val acc = onCollisionWithProjectile
          (enemy, projectileTree, acc, walls, wallTree, platforms, platformTree)
      in
        filterProjectileCollisions
          ( pos - 1
          , enemies
          , projectileTree
          , acc
          , walls
          , wallTree
          , platforms
          , platformTree
          )
      end

  (* removes enemies from `enemies` vector when player is attacking that enemy
   * and also filter enemy (or change enemyh health)
   * if enemy has collided with projectile *)
  fun filterWhenAttacked
    ( pos
    , collisions
    , enemies
    , projectileTree
    , acc
    , walls
    , wallTree
    , platforms
    , platformTree
    ) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)
        val acc =
          if exists (#id enemy, collisions) then (* filter out *)
            acc
          else
            onCollisionWithProjectile
              ( enemy
              , projectileTree
              , acc
              , walls
              , wallTree
              , platforms
              , platformTree
              )
      in
        filterWhenAttacked
          ( pos - 1
          , collisions
          , enemies
          , projectileTree
          , acc
          , walls
          , wallTree
          , platforms
          , platformTree
          )
      end

  fun helpGenerateTree (pos, enemyVec, acc) =
    if pos = Vector.length enemyVec then
      acc
    else
      let
        val {id, x, y, health = _, xAxis = _, yAxis = _} =
          Vector.sub (enemyVec, pos)

        val size = Constants.enemySize
        val ww = Constants.worldWidth
        val wh = Constants.worldHeight

        val acc = QuadTree.insert (x, y, size, size, 0, 0, ww, wh, id, acc)
      in
        helpGenerateTree (pos + 1, enemyVec, acc)
      end

  fun generateTree enemyVec = helpGenerateTree (0, enemyVec, QuadTree.empty)

  fun helpFind (findNum, vec, low, high) =
    (* should only be called when we know enemy already exists in vec *)
    let
      val mid = low + ((high - low) div 2)
      val enemy = Vector.sub (vec, mid)
      val {id = curNum, x = _, y = _, health = _, xAxis = _, yAxis = _} = enemy
    in
      if curNum = findNum then enemy
      else if curNum < findNum then helpFind (findNum, vec, mid + 1, high)
      else helpFind (findNum, vec, low, mid - 1)
    end

  fun find (findNum, vec) =
    helpFind (findNum, vec, 0, Vector.length vec - 1)

  fun helpGetDrawVec (enemy, width, height) =
    let
      val {x, y, id = _, health = _, xAxis = _, yAxis = _} = enemy
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
