structure TraceJump =
struct
  structure Trace = MakeQuadTreeFold (struct
    type env = int
    type state = bool

    fun fold (foldPlatID, nextPlatID, hasFoundNextPlatID) =
      hasFoundNextPlatID orelse foldPlatID = nextPlatID
  end)

  fun traceRightJumpDescent (x, y, nextPlatID, platTree) =
    if x >= Constants.worldWidth orelse y >= Constants.worldHeight then
      (* we hit bounds of screen and saw that there was 
       * no way to jump to next nextPlatID *)
      false
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpRight =
          Trace.foldRegion (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y + Constants.moveEnemyBy
      in
        shouldJumpRight orelse 
        traceRightJumpDescent (nextX, nextY, nextPlatID, platTree)
      end

  fun traceRightDrop (enemy, nextPlatID, platTree) =
    let
      open GameType
      val {x, y, ...}: enemy = enemy
      val x = x - Constants.enemySize
    in
      traceRightJumpDescent (x, y, nextPlatID, platTree)
    end

  fun traceRightJumpAscent (x, y, remainingJump, nextPlatID, platTree) =
    if remainingJump >= Constants.jumpLimit then
      traceRightJumpDescent (x, y, nextPlatID, platTree)
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val shouldJumpRight =
          Trace.foldRegion (x, y, width, height, nextPlatID, false, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y - Constants.moveEnemyBy
        val nextJump = remainingJump + Constants.moveEnemyBy
      in
        shouldJumpRight orelse
        traceRightJumpAscent (nextX, nextY, nextJump, nextPlatID, platTree)
      end

  fun traceRightJump (enemy, nextPlatID, platTree) =
    let
      open GameType
      val {x, y, ...}: enemy = enemy
      val x = x - Constants.enemySize
    in
      if EnemyPhysics.standingOnArea (x, y, platTree) then
        traceRightJumpAscent (x, y, 0, nextPlatID, platTree)
      else
        case #yAxis enemy of
          JUMPING amt =>
            traceRightJumpAscent (x, y, amt, nextPlatID, platTree)
        | ON_GROUND =>
            traceRightJumpAscent (x, y, 0, nextPlatID, platTree)
        | FALLING =>
            traceRightJumpDescent (x, y, nextPlatID, platTree)
        | DROP_BELOW_PLATFORM =>
            traceRightJumpDescent (x, y, nextPlatID, platTree)
        | FLOATING _ =>
            traceRightJumpDescent (x, y, nextPlatID, platTree)
    end
end
