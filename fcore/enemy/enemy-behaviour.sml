structure EnemyBehaviour =
struct
  open EnemyType
  open EntityType

  (* if player is attacking, does enemy collide with attack? *)
  fun isCollidingWithPlayerAttack (player: PlayerType.player, enemy: enemy) =
    let
      val {x = px, y = py, facing, mainAttack, ...} = player
      val pSize = Constants.playerSize

      val {x = ex, y = ey, ...} = enemy
      val eSize = Constants.enemySize
    in
      case mainAttack of
        PlayerType.MAIN_ATTACKING {length, ...} =>
          (case facing of
             FACING_RIGHT =>
               let
                 val px = px + pSize
               in
                 Collision.isCollidingPlus
                   (px, py, length, pSize, ex, ey, eSize, eSize)
               end
           | FACING_LEFT =>
               let
                 val px = px - length
               in
                 Collision.isCollidingPlus
                   (px, py, length, pSize, ex, ey, eSize, eSize)
               end)
      | _ => false
    end

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

  fun getPatrolPatches (enemy: enemy, wallTree, platformTree, acc) =
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
              EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT
              :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then
              (* invert direction if moving further left 
               * will result in falling down  *)
              acc
            else
              EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT
              :: acc
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
              EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT
              :: acc
            else if canWalkAhead (searchStartX, y, wallTree, platformTree) then
              (* invert direction if moving further right
               * will result in falling down  *)
              acc
            else
              EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT
              :: acc
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
          in
            case eyAxis of
              ON_GROUND => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
            | FALLING => EnemyPatch.W_Y_AXIS (JUMPING 0) :: acc
            | _ => acc
          end
        else (* have to travel either left or right before jumping *) if
          ecx < platX
        then
          EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT
          :: acc
        else
          EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT
          :: acc
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
          let in
            case eyAxis of
              ON_GROUND => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
            | FALLING => EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM :: acc
            | _ => acc
          end
        else (* have to travel either left or right before jumping *) if
          ecx < platX
        then
          EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT
          :: acc
        else
          EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT
          :: acc
      else
        acc
    end

  fun getMoveRightPatches (nextPlatform, enemy, platformTree, acc) =
    (* important to check for drop first because path of traceRightJump includes
     * descent of jump/drop.
     * So, if we check for jump first, we would always jump before dropping
     * even if jumping is not necessary. *)
    if TraceJump.traceRightDrop (enemy, #id nextPlatform, platformTree) then
      EnemyPatch.W_FACING FACING_RIGHT
      :: EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM
      :: EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
    else if TraceJump.traceRightJump (enemy, #id nextPlatform, platformTree) then
      if standingOnArea (enemy, platformTree) then
        EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_Y_AXIS (JUMPING 0)
        :: EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
      else
        EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT
        :: acc
    else
      EnemyPatch.W_FACING FACING_RIGHT :: EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc

  fun getMoveLeftPatches (nextPlatform, enemy, platformTree, acc) =
    if TraceJump.traceLeftDrop (enemy, #id nextPlatform, platformTree) then
      EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_Y_AXIS DROP_BELOW_PLATFORM
      :: EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
    else if TraceJump.traceLeftJump (enemy, #id nextPlatform, platformTree) then
      if standingOnArea (enemy, platformTree) then
        EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_Y_AXIS (JUMPING 0)
        :: EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
      else
        EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT :: acc
    else
      EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT :: acc

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
  fun getHorizontalLandingPatches (enemy, nextPlatform, acc) = acc

  fun getFallingPatches (enemy, newPlatformID, platforms, acc) =
    EnemyPatch.W_NEXT_PLAT_ID ~1 :: acc

  fun getJumpLandingPatches (enemy, nextPlatformID, platforms, acc) = acc

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
        (case #facing enemy of
           FACING_RIGHT => EnemyPatch.W_X_AXIS MOVE_RIGHT :: acc
         | FACING_LEFT => EnemyPatch.W_X_AXIS MOVE_LEFT :: acc)
    | _ => getPatrolPatches (enemy, wallTree, platformTree, acc)

  fun isInFollowRange (player, enemy) =
    let
      val {x = px, y = py, ...} = player
      val pfx = px + Constants.playerSize
      val pfy = py + Constants.playerSize

      val range = 199

      val {x = ex, y = ey, ...} = enemy
      val eStartX = ex - range
      val eStartY = ey - range
      val efx = ex + Constants.enemySize + range
      val efy = ey + Constants.enemySize + range
    in
      Collision.isColliding (px, py, pfx, pfy, eStartX, eStartY, efx, efy)
    end

  fun getFollowPatches
    ( player: PlayerType.player
    , enemy
    , wallTree
    , platformTree
    , platforms
    , graph
    , acc
    ) =
    let
      val pID = #platID player
      val eID = #platID enemy
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
      else if isInFollowRange (player, enemy) then
        (* line of sight: only follow player if player is in some range *)
        let
          val bestPath = PathFinding.start
            (pID, eID, platforms, platformTree, graph)
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
      else
        startPatrolPatches (player, enemy, wallTree, platformTree, acc)
    end

  fun withDefaultYAxis (enemy: enemy) =
    case #yAxis enemy of
      ON_GROUND => EnemyPatch.withPatch (enemy, EnemyPatch.W_Y_AXIS FALLING)
    | _ => enemy

  fun updatePatrolState
    (player, enemy, walls, wallTree, platforms, platformTree) =
    let
      val {x, y, ...} = enemy
      val size = Constants.enemySize
      val enemy = withDefaultYAxis enemy

      val patches = getPatrolPatches (enemy, wallTree, platformTree, [])
      val enemy = EnemyPatch.withPatches (enemy, patches)

      val patches = EnemyPhysics.getPhysicsPatches enemy
      val enemy = EnemyPatch.withPatches (enemy, patches)

      val patches = EnemyPhysics.getEnvironmentPatches
        (enemy, walls, wallTree, platforms, platformTree)
    in
      EnemyPatch.withPatches (enemy, patches)
    end

  fun updateFollowState
    (player, enemy, walls, wallTree, platforms, platformTree, graph) =
    let
      val {x, y, ...} = enemy
      val size = Constants.enemySize

      val enemy = withDefaultYAxis enemy

      val patches = getFollowPatches
        (player, enemy, wallTree, platformTree, platforms, graph, [])
      val enemy = EnemyPatch.withPatches (enemy, patches)

      val patches = EnemyPhysics.getPhysicsPatches enemy
      val enemy = EnemyPatch.withPatches (enemy, patches)

      val patches = EnemyPhysics.getEnvironmentPatches
        (enemy, walls, wallTree, platforms, platformTree)
    in
      EnemyPatch.withPatches (enemy, patches)
    end

  fun updateStraightBat (player, enemy, walls, wallTree) =
    let
      val {x, y, batRest, batDirY, batMinY, batMaxY, xAxis, ...} = enemy

      val size = Constants.enemySize
      val moveByY = Constants.moveBatY
      val moveByX = Constants.moveBatX

      val patches =
        (* get apatches for up/down movement *)
        case batDirY of
          UP =>
            if y - moveByY <= batMaxY then
              [EnemyPatch.W_BAT_DIR_Y DOWN, EnemyPatch.W_Y (y + moveByY)]
            else
              [EnemyPatch.W_Y (y - moveByY)]
        | DOWN =>
            if y + moveByY >= batMinY then
              [EnemyPatch.W_BAT_DIR_Y UP, EnemyPatch.W_Y (y - moveByY)]
            else
              [EnemyPatch.W_Y (y + moveByY)]

      val patches =
        (* get patches for horizontal movement *)
        if QuadTree.hasCollisionAt (x, y, size, size, ~1, wallTree) then
          (* has collision with wall *)
          if batRest >= Constants.batRestLimit then
            (* make enemy move in opposite direction *)
            case xAxis of
              MOVE_RIGHT =>
                EnemyPatch.W_FACING FACING_LEFT :: EnemyPatch.W_X_AXIS MOVE_LEFT
                :: EnemyPatch.W_X (x - 1) :: patches
            | MOVE_LEFT =>
                EnemyPatch.W_FACING FACING_RIGHT
                :: EnemyPatch.W_X_AXIS MOVE_RIGHT :: EnemyPatch.W_X (x + 1)
                :: patches
            | _ => patches
          else
            (* keep resting until we hit rest limit *)
            EnemyPatch.W_BAT_REST (batRest + 1) :: patches
        else
          (* no collision, so continue moving in direction *)
          let
            val patches =
              case xAxis of
                MOVE_RIGHT => EnemyPatch.W_X (x + moveByX) :: patches
              | MOVE_LEFT => EnemyPatch.W_X (x - moveByX) :: patches
              | STAY_STILL => patches
          in
            EnemyPatch.W_BAT_REST 0 :: patches
          end
    in
      EnemyPatch.withPatches (enemy, patches)
    end

  fun updateEnemyState
    (enemy, walls, wallTree, platforms, platformTree, player, graph) =
    case #variant enemy of
      PATROL_SLIME =>
        updatePatrolState
          (player, enemy, walls, wallTree, platforms, platformTree)
    | FOLLOW_SLIME =>
        updateFollowState
          (player, enemy, walls, wallTree, platforms, platformTree, graph)
    | STRAIGHT_BAT => updateStraightBat (player, enemy, walls, wallTree)
end
