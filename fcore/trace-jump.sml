structure TraceJump =
struct
  structure Trace =
    MakeQuadTreeFold
      (struct
         type env = int
         type state = bool

         fun fold (foldPlatID, nextPlatID, hasFoundNextPlatID) =
           hasFoundNextPlatID orelse foldPlatID = nextPlatID
       end)

  fun traceRightDescent (x, y, nextPlatID, platTree) =
    if x >= Constants.worldWidth orelse y >= Constants.worldHeight then
      (* we hit bounds of screen and saw that there was 
       * no way to jump to next nextPlatID *)
      false
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpRight = Trace.foldRegion
          (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y + Constants.moveEnemyBy
      in
        shouldJumpRight
        orelse traceRightDescent (nextX, nextY, nextPlatID, platTree)
      end

  fun traceRightDrop (enemy: EnemyType.enemy, nextPlatID, platTree) =
    let
      val {x, y, ...} = enemy
      val x = x - Constants.enemySize
    in
      traceRightDescent (x, y, nextPlatID, platTree)
    end

  fun traceRightJumpAscent (x, y, remainingJump, nextPlatID, platTree) =
    (* because value of y parameter is at the top, 
     * we subtract the jump limit by the enemy's size,
     * so we only check for places enemy can jump to. *)
    if remainingJump >= Constants.jumpLimit - Constants.enemySize then
      traceRightDescent (x, y, nextPlatID, platTree)
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpRight = Trace.foldRegion
          (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y - Constants.moveEnemyBy
        val nextJump = remainingJump + Constants.moveEnemyBy
      in
        shouldJumpRight
        orelse
        traceRightJumpAscent (nextX, nextY, nextJump, nextPlatID, platTree)
      end

  fun traceRightJump (enemy: EnemyType.enemy, nextPlatID, platTree) =
    let
      val {x, y, ...} = enemy
      val x = x - Constants.enemySize

      open EntityType
    in
      if EnemyPhysics.standingOnArea (x, y, platTree) then
        traceRightJumpAscent (x, y, 0, nextPlatID, platTree)
      else
        case #yAxis enemy of
          JUMPING amt => traceRightJumpAscent (x, y, amt, nextPlatID, platTree)
        | ON_GROUND => traceRightJumpAscent (x, y, 0, nextPlatID, platTree)
        | FALLING => traceRightDescent (x, y, nextPlatID, platTree)
        | DROP_BELOW_PLATFORM => traceRightDescent (x, y, nextPlatID, platTree)
        | FLOATING _ => traceRightDescent (x, y, nextPlatID, platTree)
    end

  fun traceLeftDescent (x, y, nextPlatID, platTree) =
    if x <= 0 orelse y >= Constants.worldHeight then
      false
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpLeft = Trace.foldRegion
          (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x - Constants.moveEnemyBy
        val nextY = y + Constants.moveEnemyBy
      in
        shouldJumpLeft
        orelse traceLeftDescent (nextX, nextY, nextPlatID, platTree)
      end

  fun traceLeftDrop (enemy: EnemyType.enemy, nextPlatID, platTree) =
    let
      val {x, y, ...} = enemy
      val x = x + Constants.enemySize
    in
      traceLeftDescent (x, y, nextPlatID, platTree)
    end

  fun traceLeftJumpAscent (x, y, remainingJump, nextPlatID, platTree) =
    if x <= 0 orelse remainingJump >= Constants.jumpLimit - Constants.enemySize then
      traceLeftDescent (x, y, nextPlatID, platTree)
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpLeft = Trace.foldRegion
          (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x - Constants.moveEnemyBy
        val nextY = y - Constants.moveEnemyBy
        val nextJump = remainingJump + Constants.moveEnemyBy
      in
        shouldJumpLeft
        orelse
        traceLeftJumpAscent (nextX, nextY, nextJump, nextPlatID, platTree)
      end

  fun traceLeftJump (enemy: EnemyType.enemy, nextPlatID, platTree) =
    let
      val {x, y, ...} = enemy
      val x = x + 75

      open EntityType
    in
      if EnemyPhysics.standingOnArea (x, y, platTree) then
        traceLeftJumpAscent (x, y, 0, nextPlatID, platTree)
      else
        case #yAxis enemy of
          JUMPING amt => traceLeftJumpAscent (x, y, amt, nextPlatID, platTree)
        | ON_GROUND => traceLeftJumpAscent (x, y, 0, nextPlatID, platTree)
        | FALLING => traceLeftDescent (x, y, nextPlatID, platTree)
        | DROP_BELOW_PLATFORM => traceLeftDescent (x, y, nextPlatID, platTree)
        | FLOATING _ => traceLeftDescent (x, y, nextPlatID, platTree)
    end
end
