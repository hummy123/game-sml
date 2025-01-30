structure EnemyBehaviour =
struct
  open GameType

  fun canWalkAhead (x, y, wallTree, platformTree) =
    let
      val y = y + Constants.enemySize - 5
      val searchHeight = 10
      val searchWidth = Constants.moveEnemyBy
    in
      QuadTree.hasCollisionAt (x, y, searchWidth, searchHeight, ~1, wallTree)
      orelse
      QuadTree.hasCollisionAt
        (x, y, searchWidth, searchHeight, ~1, platformTree)
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
    in
      QuadTree.hasCollisionAt (ex, ey, width, height, ~1, tree)
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
            val searchWidth = Constants.moveEnemyBy
            val searchHeight = Constants.enemySize - 5

            val hasWallAhead = QuadTree.hasCollisionAt
              (searchStartX, y, searchWidth, searchHeight, ~1, wallTree)
          in
            if hasWallAhead then
              EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then
              (* invert direction if moving further left 
               * will result in falling down  *)
              acc
            else
              EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
          end
      | MOVE_RIGHT =>
          let
            (* enemy's x field is top left coordinate 
             * but we want to check top * right coordinate, 
             * so add enemySize *)
            val searchStartX = x + Constants.enemySize + Constants.moveEnemyBy
            val searchWidth = Constants.moveEnemyBy
            val searchHeight = Constants.enemySize - 5

            val hasWallAhead = QuadTree.hasCollisionAt
              (searchStartX, y, searchWidth, searchHeight, ~1, wallTree)
          in
            if hasWallAhead then
              EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then
              (* invert direction if moving further right
               * will result in falling down  *)
              acc
            else
              EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
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

  fun getPlatformBelowEnemy (enemy: enemy, platformTree, platforms) =
    let
      val {x, y, ...} = enemy

      val searchWidth = Constants.enemySize
      val searchHeight = Constants.worldHeight - y

      val y = y + Constants.enemySize

      val collisions = QuadTree.getCollisions
        (x, y, searchWidth, searchHeight, ~1, platformTree)
      val wh = Constants.worldHeight
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
          let
            (* since we want to jump vertically and not risk falling off by
             * jumping + moving either left or right, make enemy stay still *)
            val acc = EnemyPatch.W_X_AXIS STAY_STILL :: acc
          in
            case eyAxis of
              ON_GROUND => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
            | FALLING => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
            | _ => acc
          end
        else (* have to travel either left or right before jumping *) if
          ecx < platX
        then
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
          let
            val acc = EnemyPatch.W_X_AXIS STAY_STILL :: acc
          in
            case eyAxis of
              ON_GROUND => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
            | FALLING => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
            | _ => acc
          end
        else (* have to travel either left or right before jumping *) if
          ecx < platX
        then
          EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
        else
          EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
      else
        acc
    end

  fun getMoveRightPatches (nextPlatform, enemy, platformTree, acc) =
    (* important to check for drop first because path of traceRightJump includes
     * descent of jump/drop.
     * So, if we check for jump first, we would always jump before dropping
     * even if jumping is not necessary. *)
    if TraceJump.traceRightDrop (enemy, #id nextPlatform, platformTree) then
      EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: EnemyPatch.W_X_AXIS MOVE_RIGHT
      :: acc
    else if TraceJump.traceRightJump (enemy, #id nextPlatform, platformTree) then
      if standingOnArea (enemy, platformTree) then
        EnemyPatch.W_Y_AXIS (JUMPING 0) :: EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
      else
        EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
    else
      EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc

  fun getMoveLeftPatches (nextPlatform, enemy, platformTree, acc) =
    if TraceJump.traceLeftDrop (enemy, #id nextPlatform, platformTree) then
      EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: EnemyPatch.W_X_AXIS MOVE_LEFT
      :: acc
    else if TraceJump.traceLeftJump (enemy, #id nextPlatform, platformTree) then
      if standingOnArea (enemy, platformTree) then
        EnemyPatch.W_Y_AXIS (JUMPING 0) :: EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
      else
        EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
    else
      EnemyPatch.W_X_AXIS MOVE_LEFT :: acc

  (* get patches to help enemy move to nextPlatformID *)
  fun getPathToNextPlatform
    (nextPlatformID, platforms, platformTree, enemy, eID, pID, acc) =
    let
      val currentPlatform = Platform.find (eID, platforms)
      val nextPlatform = Platform.find (nextPlatformID, platforms)

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
          val {x = eX, ...} = enemy
          val {x = nPlatX, width = nPlatW, ...} = nextPlatform
        in
          if eX < nPlatX then
            getMoveRightPatches (nextPlatform, enemy, platformTree, acc)
          else
            getMoveLeftPatches (nextPlatform, enemy, platformTree, acc)
        end
    end

  (* if only one side in x direction overlaps with platform,
   * then move enemy left/right to make them fully overlap with platform *)
  fun getHorizontalLandingPatches (enemy, nextPlatform, acc) =
    case #xAxis enemy of
      STAY_STILL => acc
    | _ =>
        let
          val {x = px, width = pw, ...} = nextPlatform
          val pfx = px + pw

          val {x = ex, ...} = enemy
          val efx = ex + Constants.enemySize
        in
          if isBetween (px, ex, pfx) andalso isBetween (px, efx, pfx) then
            acc
          else
            let
              val startDiff = abs (px - ex)
              val endDiff = abs (pfx - efx)
            in
              if startDiff > endDiff then EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
              else EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
            end
        end

  fun getFallingPatches (enemy, newPlatformID, platforms, acc) =
    let
      val nextPlatform = Platform.find (newPlatformID, platforms)
      val acc = getHorizontalLandingPatches (enemy, nextPlatform, acc)
    in
      EnemyPatch.W_NEXT_PLAT_ID ~1 :: acc
    end

  fun getJumpLandingPatches (enemy, nextPlatformID, platforms, acc) =
    let
      val nextPlatform = Platform.find (nextPlatformID, platforms)
      val {y = py, ...} = nextPlatform

      val {y = ey, ...} = enemy

      val acc = getHorizontalLandingPatches (enemy, nextPlatform, acc)
    in
      if ey < py - 65 then
        (* set to falling *)
        EnemyPatch.W_NEXT_PLAT_ID ~1 :: EnemyPatch.W_Y_AXIS FALLING :: acc
      else
        acc
    end

  fun getLandingPatches (newPlatformID, platforms, enemy, acc) =
    case #yAxis enemy of
      JUMPING _ => getJumpLandingPatches (enemy, newPlatformID, platforms, acc)
    | _ => getFallingPatches (enemy, newPlatformID, platforms, acc)

  (* to be called by FOLLOW_SIME. The FOLLOW_SIME sometimes changes its x axis
   * to STAY_STILL, so if this happens and we want to patrol,
   * then start patrolling in the direction the player is in *)
  fun startPatrolPatches (player, enemy, wallTree, platformTree, acc) =
    case #xAxis enemy of
      STAY_STILL =>
        let
          val acc =
            if #x player <= #x enemy then EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
            else EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
        in
          acc
        end
    | _ => getPatrollPatches (enemy, wallTree, platformTree, acc)

  fun getFollowPatches
    (player: player, enemy, wallTree, platformTree, platforms, acc) =
    let
      (* todo: possibly get pID and eID of player/enemy in a different way *)
      val pID = #platID player

      val eID = getPlatformBelowEnemy (enemy, platformTree, platforms)
      val eID = if eID = ~1 then #platID enemy else eID

    in
      if eID = ~1 orelse pID = ~1 then
        (* without checking that neither of these are ~1 
        * (which means there is no platform below the enemy/player)
        * there is a subscript error because the PathFinding.start
        * function expects neither of these values to be ~1. *)
        startPatrolPatches (player, enemy, wallTree, platformTree, acc)
      else if eID = #nextPlatID enemy then
        getLandingPatches (eID, platforms, enemy, acc)
      else if eID = pID then
        startPatrolPatches (player, enemy, wallTree, platformTree, acc)
      else
        let
          val bestPath = PathFinding.start (pID, eID, platforms, platformTree)
        in
          case bestPath of
            nextPlatformID :: _ =>
              let
                val acc = EnemyPatch.W_NEXT_PLAT_ID nextPlatformID :: acc
                val acc = getPathToNextPlatform
                  ( nextPlatformID
                  , platforms
                  , platformTree
                  , enemy
                  , eID
                  , pID
                  , acc
                  )
              in
                EnemyPatch.W_X_AXIS STAY_STILL :: acc
              end
          | [] =>
              startPatrolPatches (player, enemy, wallTree, platformTree, acc)
        end
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
