structure EnemyBehaviour =
struct
  open GameType

  fun canWalkAhead (x, y, wallTree, platformTree) =
    let
      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val searchWidth = Constants.enemySize

      val y = y + Constants.enemySize - 5
      val searchHeight = 10
    in
      QuadTree.hasCollisionAt
        (x, y, searchWidth, searchHeight, 0, 0, ww, wh, ~1, wallTree)
      orelse
      QuadTree.hasCollisionAt
        (x, y, searchWidth, searchHeight, 0, 0, ww, wh, ~1, platformTree)
    end

  (* same function takes either wallTree or platformTree and returns true
   * if standing on tree.
   * Function is monomorphic in the sense that wallTree and platformTree 
   * are both same type (no generics/parametric polymorphism).
   * *)
  fun standingOnArea (enemy: enemy, tree) =
    let
      val {x = ex, y = ey, ...} = enemy

      val ey = ey + Constants.enemySize - 1

      val width = Constants.enemySize
      val height = Platform.platHeight

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight
    in
      QuadTree.hasCollisionAt (ex, ey, width, height, 0, 0, ww, wh, ~1, tree)
    end

  fun getPatrollPatches (enemy: enemy, wallTree, platformTree, acc) =
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

      val {x, y, xAxis, ...} = enemy
    in
      case xAxis of
        MOVE_LEFT =>
          let
            (* search to see if there is wall on left side *)
            val searchStartX = x - Constants.moveEnemyBy
            val searchWidth = Constants.enemySize
            val searchHeight = Constants.enemySize - 5

            val ww = Constants.worldWidth
            val wh = Constants.worldHeight

            val hasWallAhead = QuadTree.hasCollisionAt
              ( searchStartX
              , y
              , searchWidth
              , searchHeight
              , 0
              , 0
              , ww
              , wh
              , ~1
              , wallTree
              )
          in
            if
              hasWallAhead
            then EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then 
              (* invert direction if moving further left 
               * will result in falling down  *) 
              acc
            else EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
          end
      | MOVE_RIGHT =>
          let
            (* enemy's x field is top left coordinate 
             * but we want to check top * right coordinate, 
             * so add enemySize *)
            val searchStartX = x + Constants.enemySize + Constants.moveEnemyBy
            val searchWidth = Constants.enemySize
            val searchHeight = Constants.enemySize - 5

            val ww = Constants.worldWidth
            val wh = Constants.worldHeight

            val hasWallAhead = QuadTree.hasCollisionAt
              ( searchStartX
              , y
              , searchWidth
              , searchHeight
              , 0
              , 0
              , ww
              , wh
              , ~1
              , wallTree
              )
          in
            if
              hasWallAhead
            then EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then 
              (* invert direction if moving further right
               * will result in falling down  *) 
              acc
            else EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
          end
      | STAY_STILL => acc
    end

  (* pathfinding *)
  fun isBetween (p1, check, p2) = check >= p1 andalso check <= p2

  fun getHighestPlatform (collisions, platforms, highestY, highestID, checkY) =
    case collisions of
      id :: tl =>
        let
          val {y = platY, ...} = Platform.find (id, platforms)
        in
          (* platY < highestY is correct because lowest number = highest 
           * in * this case *)
          if platY < highestY andalso checkY <= platY then 
            getHighestPlatform (tl, platforms, platY, id, checkY)
          else 
            getHighestPlatform (tl, platforms, highestY, highestID, checkY)
        end
    | [] => highestID

  fun getPlatformBelowPlayer (player, platformTree, platforms) =
    let
      val {x, y, ...} = player

      val searchWidth = Constants.playerSize
      val searchHeight = Constants.worldHeight - y

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val collisions = QuadTree.getCollisions
        (x, y, searchWidth, searchHeight, 0, 0, ww, wh, ~1, platformTree)
      val checkY = y + Constants.playerSize
    in
      getHighestPlatform (collisions, platforms, wh, ~1, checkY)
    end

  fun getPlatformBelowEnemy (enemy: enemy, platformTree, platforms) =
    let
      val {x, y, ...} = enemy

      val searchWidth = Constants.enemySize
      val searchHeight = Constants.worldHeight - y

      val y = y + Constants.enemySize

      val ww = Constants.worldWidth
      val wh = Constants.worldHeight

      val collisions = QuadTree.getCollisions
        (x, y, searchWidth, searchHeight, 0, 0, ww, wh, ~1, platformTree)
    in
      getHighestPlatform (collisions, platforms, wh, ~1, y)
    end

  fun canJump (prevPlatform, nextPlatform) =
    let
      val {x = pPlatX, y = pPlatY, width = pPlatW, ...} = prevPlatform
      val pPlatFinishX = pPlatX + pPlatW

      val {x = nPlatX, y = nPlatY, width = nPlatW, ...} = nextPlatform
      val nPlatFinishX = nPlatX + nPlatW
    in
      (isBetween (nPlatX, pPlatX, nPlatFinishX)
       orelse isBetween (nPlatX, pPlatFinishX, nPlatFinishX))
      andalso pPlatY > nPlatY
    end

  fun getJumpPatches (nextPlatform, platformTree, enemy, acc) =
    let
      val {x = platX, y = platY, width = platWidth, ...} = nextPlatform
      val platFinishX = platX + platWidth

      val {x = eX, y = ey, yAxis = eyAxis, xAxis = exAxis, ...} = enemy
      val ecx = eX + (Constants.enemySize div 2)
      val ey = ey + Constants.enemySize

      val standingOnPlat = standingOnArea (enemy, platformTree)
    in
      if ey >= platY andalso standingOnPlat then
        if
          isBetween (platX, ecx, platFinishX)
        then
          (* can jump from same position enemy is at *)
          case eyAxis of
            ON_GROUND => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
          | FALLING => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
          | _ => acc
        else 
          (* have to travel either left or right before jumping *) 
          if ecx < platX then
            EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
          else
            EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
      else
        acc
    end

  fun canDrop (prevPlatform, nextPlatform) =
    let
      val {x = pPlatX, y = pPlatY, width = pPlatW, ...} = prevPlatform
      val pPlatFinishX = pPlatX + pPlatW

      val {x = nPlatX, y = nPlatY, width = nPlatW, ...} = nextPlatform
      val nPlatFinishX = nPlatX + nPlatW
    in
      (isBetween (nPlatX, pPlatX, nPlatFinishX)
       orelse isBetween (nPlatX, pPlatFinishX, nPlatFinishX))
      andalso pPlatY < nPlatY
    end

  fun getDropPatches (nextPlatform, platformTree, enemy, acc) =
    let
      val {x = platX, y = platY, width = platWidth, ...} = nextPlatform
      val platFinishX = platX + platWidth

      val {x = eX, y = ey, yAxis = eyAxis, xAxis = exAxis, ...} = enemy
      val ecx = eX + (Constants.enemySize div 2)
      val ey = ey + Constants.enemySize

      val standingOnPlat = standingOnArea (enemy, platformTree)
    in
      if ey <= platY andalso standingOnPlat then
        if
          isBetween (platX, ecx, platFinishX)
        then
          (* can jump from same position enemy is at *)
          case eyAxis of
            ON_GROUND => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
          | FALLING => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
          | _ => acc
        else 
          (* have to travel either left or right before jumping *) 
          if ecx < platX then
            EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
          else
            EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
      else
        acc
    end

  fun getMoveRightPatches (nextPlatform, enemy, platformTree, acc) =
    let
      val {x = platX, y = platY, width = platWidth, ...} = nextPlatform
      val platFinishX = platX + platWidth

      val {x = ex, y = ey, yAxis = eyAxis, ...} = enemy

      val xDiff = platX - ex
    in
      if ey > platY then
        (* enemy is lower than next platform so needs to jump *)
        let
          val jumpAmt =
            case eyAxis of
              JUMPING amt => amt
            | _ => 0
          val apexY = ey - (Constants.jumpLimit - jumpAmt)

          (* enemy moves in x and y axis at same rate 
           * with no acceleration or deceleration. 
           * So, we can directly compare to see which is lower;
           * if x is lower, that means we can't reach if we jump at this point
           * but if y is lower, that means we can reach if we jump at this point
           * so we should simply move rightwards.
           * *)
          val xyDiff = apexY - xDiff
        in
          if xyDiff >= 0 then
            let
              val acc =
                case eyAxis of
                  ON_GROUND => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
                | _ => acc
            in
              EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
            end
          else
              EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
        end
      else
        (* platform is below or at same y coordinat as enemy 
         * so might possibly require dropping below rather than jumping. *)
        let

        in
          EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
        end
    end

  (* get patches to help enemy move to nextPlatformID *)
  fun getPathToNextPlatform
    (nextPlatformID, platforms, platformTree, enemy, eID, pID, acc) =
    let
      val currentPlatform = Platform.find (eID, platforms)
      val nextPlatform = Platform.find (nextPlatformID, platforms)

      val {x = eX, y = ey, yAxis = eyAxis, ...} = enemy

      val canJump = canJump (currentPlatform, nextPlatform)
      val canDrop = canDrop (currentPlatform, nextPlatform)
    in
      if canJump then
        getJumpPatches (nextPlatform, platformTree, enemy, acc)
      else if canDrop then
        getDropPatches (nextPlatform, platformTree, enemy, acc)
      else 
        let
          (* if can neither jump or drop to next platform vertically
           * then remaining options are either jumping to the right or left.
           * Figure out which the enemy needs to do and progress to it. *)
          val {x = nPlatX, width = nPlatW, ...} = nextPlatform
        in
          if eX < nPlatX then
            getMoveRightPatches (nextPlatform, enemy, platformTree, acc)
          else
            (* move to the left *)
            EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
        end
    end

  fun canJumpOnPlatform (player, platforms, enemy: enemy, platformTree, acc) =
    let
      (* todo: possibly get pID and eID of player/enemy in a different way *)
      val pID = getPlatformBelowPlayer (player, platformTree, platforms)
      val eID = getPlatformBelowEnemy (enemy, platformTree, platforms)
    in
      if eID = pID then
        EnemyPatch.W_Y_AXIS FALLING :: acc
      else if eID = ~1 orelse pID = ~1 then
        (* without checking that neither of these are ~1 
        * (which means there is no platform below the enemy/player)
        * there is a subscript error because the PathFinding.start
        * function expects neither of these values to be ~1. *)
        acc
      else
        let
          val bestPath = PathFinding.start (pID, eID, platforms, platformTree)
        in
          case bestPath of
            nextPlatformID :: _ =>
              getPathToNextPlatform
                (nextPlatformID, platforms, platformTree, enemy, eID, pID, acc)
          | [] => acc
        end
    end

  fun getFollowPatches
    (player: player, enemy, wallTree, platformTree, platforms, acc) =
    let
      val {x = px, y = py, ...} = player
      val {x = ex, y = ey, yAxis = eyAxis, ...} = enemy

      val xAxis = if px < ex then MOVE_LEFT else MOVE_RIGHT

      val acc = canJumpOnPlatform (player, platforms, enemy, platformTree, acc)

    in
      EnemyPatch.W_X_AXIS STAY_STILL :: acc
    end

  fun getVariantPatches
    (enemy, walls, wallTree, platforms, platformTree, player, acc) =
    let
      open EnemyVariants
    in
      case #variant enemy of
        PATROL_SLIME => getPatrollPatches (enemy, wallTree, platformTree, acc)
      | FOLLOW_SIME =>
          getFollowPatches
            (player, enemy, wallTree, platformTree, platforms, acc)
    end
end
