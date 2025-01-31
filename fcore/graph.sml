structure Graph =
struct
  fun insertIfNew (dist, platSet, foldPlatID) =
    let
      val insRecord = {id = foldPlatID, distance = dist}
      val pos = PlatSet.findInsPos (insRecord, platSet)
    in
      if pos <> ~1 andalso pos <> Vector.length platSet then
        (* platSet may already contain foldPlatID; check *)
        let
          val {id = key, ...} = PlatSet.sub (platSet, pos)
        in
          if key = foldPlatID then
            (* alread contains foldPlatID so return *)
            platSet
          else
            (* foldPlatID is new; insert it *)
            PlatSet.insert (platSet, insRecord, pos)
        end
      else
        (* foldPlatID is new; insert it *)
        PlatSet.insert (platSet, insRecord, pos)
    end

  type env =
    {platforms: GameType.platform vector, currentPlat: GameType.platform}

  structure Vertical =
    MakeQuadTreeFold
      (struct
         type env = env

         type state = PlatSet.elem vector

         fun fold (foldPlatID, env: env, platSet) =
           let
             val {platforms, currentPlat} = env

             val {y = foldPlatY, ...} = Platform.find (foldPlatID, platforms)
             val {y = currentPlatY, ...} = currentPlat
             val newDist = abs (foldPlatY - currentPlatY)
           in
             insertIfNew (newDist, platSet, foldPlatID)
           end
       end)

  structure Horizontal =
    MakeQuadTreeFold
      (struct
         type env = env

         type state = PlatSet.elem vector

         fun minWidth (p1: GameType.platform, p2: GameType.platform) =
           let
             val {x = p1x, width = p1w, ...} = p1
             val {x = p2x, width = p2w, ...} = p2

             val p1fx = p1x + p1w
             val p2fx = p2x + p2w

             val w1 = abs (p1fx - p2fx)
             val w2 = abs (p1fx - p2x)
             val w3 = abs (p1x - p2x)
             val w4 = abs (p1x - p2fx)

             val min = Int.min (w1, w2)
             val min = Int.min (min, w3)
           in
             Int.min (min, w4)
           end

         fun pythagoras (width, height) =
           let
             val wsq = width * width
             val hsq = height * height
             val hypotenuseSq = wsq + hsq
             val hypSq = Real.fromInt hypotenuseSq
             val hyp = Math.sqrt hypSq
           in
             Real.toInt IEEEReal.TO_NEAREST hyp
           end

         fun fold (foldPlatID, env: env, platSet) =
           let
             val {platforms, currentPlat} = env

             val foldPlat = Platform.find (foldPlatID, platforms)
             val foldPlatY = #y foldPlat
             val {y = currentPlatY, ...} = currentPlat

             val height = abs (foldPlatY - currentPlatY)
             val width = minWidth (currentPlat, foldPlat)

             val newDist = pythagoras (width, height)
           in
             insertIfNew (newDist, platSet, foldPlatID)
           end
       end)

  fun traceRightDescent (x, y, platTree, env, platSet) =
    if x >= Constants.worldWidth orelse y >= Constants.worldHeight then
      (* we hit bounds of screen and saw that there was 
       * no way to jump to next nextPlatID *)
      platSet
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val platSet = Horizontal.foldRegion
          (x, y, width, height, env, platSet, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y + Constants.moveEnemyBy
      in
        traceRightDescent (nextX, nextY, platTree, env, platSet)
      end

  fun traceRightJumpAscent (x, y, remainingJump, platTree, env, platSet) =
    if remainingJump >= Constants.jumpLimit - Constants.enemySize then
      traceRightDescent (x, y, platTree, env, platSet)
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y

        val platSet = Horizontal.foldRegion
          (x, y, width, height, env, platSet, platTree)

        val nextX = x + Constants.moveEnemyBy
        val nextY = y - Constants.moveEnemyBy
        val nextJump = remainingJump + Constants.moveEnemyBy
      in
        traceRightJumpAscent (nextX, nextY, nextJump, platTree, env, platSet)
      end

  fun traceRightJump (currentPlat: GameType.platform, env, platSet, platTree) =
    let
      val {x, y, width, ...} = currentPlat
      val x = x - Constants.enemySize + width
    in
      traceRightJumpAscent (x, y, 0, platTree, env, platSet)
    end

  fun traceLeftDescent (x, y, platTree, env, platSet) =
    if x <= 0 orelse y >= Constants.worldHeight then
      platSet
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y
        val platSet = Horizontal.foldRegion
          (x, y, width, height, env, platSet, platTree)

        val nextX = x - Constants.moveEnemyBy
        val nextY = y + Constants.moveEnemyBy
      in
        traceLeftDescent (nextX, nextY, platTree, env, platSet)
      end

  fun traceLeftJumpAscent (x, y, remainingJump, platTree, env, platSet) =
    if remainingJump >= Constants.jumpLimit - Constants.enemySize then
      traceLeftDescent (x, y, platTree, env, platSet)
    else
      let
        val width = Constants.moveEnemyBy
        val height = Constants.worldHeight - y

        val platSet = Horizontal.foldRegion
          (x, y, width, height, env, platSet, platTree)

        val nextX = x - Constants.moveEnemyBy
        val nextY = y - Constants.moveEnemyBy
        val nextJump = remainingJump + Constants.moveEnemyBy
      in
        traceLeftJumpAscent (nextX, nextY, nextJump, platTree, env, platSet)
      end

  fun traceLeftJump (currentPlat: GameType.platform, env, platSet, platTree) =
    let
      val {x, y, ...} = currentPlat
      val x = x + Constants.enemySize
    in
      traceLeftJumpAscent (x, y, 0, platTree, env, platSet)
    end

  fun start (currentPlat: GameType.platform, env: env, platSet, platformTree) =
    let
      val {x, y, width, ...} = currentPlat

      (* calculate area to search in y axis *)
      val searchY = y - Constants.jumpLimit
      val height = Constants.worldHeight - searchY

      val platSet = Vertical.foldRegion
        (x, searchY, width, height, env, platSet, platformTree)

      val platSet = traceRightJump (currentPlat, env, platSet, platformTree)
    in
      traceLeftJump (currentPlat, env, platSet, platformTree)
    end

  fun build (currentPlat, platforms, platformTree) =
    let
      val env = {currentPlat = currentPlat, platforms = platforms}
    in
      start (currentPlat, env, PlatSet.empty, platformTree)
    end

  fun fromPlatforms (platforms: GameType.platform vector, platformTree) =
    Vector.map (fn plat => build (plat, platforms, platformTree)) platforms
end
