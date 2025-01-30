structure BuildGraph =
struct
  fun insertIfNotExistsOrShorter (dist, eKeys, eVals, foldPlatID, q, fromPlatID) =
    let
      val pos = IntSet.findInsPos (foldPlatID, eKeys)
    in
      if pos <> ~1 andalso pos <> Vector.length eKeys then
        let
          val key = IntSet.sub (eKeys, pos)
        in
          if pos = key then
            (* may need to update record in eVals if it is shorter *)
            let
              val {distance = oldDist, ...} = ValSet.sub (eVals, pos)
            in
              if dist < oldDist then
                (* update values as we found a shorter path *)
                let
                  val eVals =
                    ValSet.updateAtIdx
                      (eVals, {distance = dist, from = fromPlatID}, pos)
                in
                  (eVals, q)
                end
              else
                (* return existing *)
                (eVals, q)
            end
          else
            (* key not explored, so add to queue *)
            let
              val insRecord =
                {distance = dist, id = foldPlatID, comesFrom = fromPlatID}
              val insPos = DistVec.findInsPos (insRecord, q)
              val q = DistVec.insert (q, insRecord, insPos)
            in
              (eVals, q)
            end
        end
      else
        (* key not explored, so add to queue *)
        let
          val insRecord =
            {distance = dist, id = foldPlatID, comesFrom = fromPlatID}
          val insPos = DistVec.findInsPos (insRecord, q)
          val q = DistVec.insert (q, insRecord, insPos)
        in
          (eVals, q)
        end
    end

  type env =
    { platforms: GameType.platform vector
    , currentPlat: GameType.platform
    , eKeys: IntSet.elem vector
    , distSoFar: int
    }

  (* adds platforms to queue if they have not been explored
   * or, if they have been explored and distance is smaller than previous, 
   * updates their distance.
   * Only intended for platforms which can be reached vertically 
   * (jumped to or dropped to without moving left or right at the same time). 
   * *)
  structure Vertical =
    MakeQuadTreeFold
      (struct
         type env = env

         type state = ValSet.elem vector * DistVec.elem vector

         fun fold (foldPlatID, env: env, (eVals, q)) =
           let
             val {platforms, currentPlat, eKeys, distSoFar} = env

             val {y = foldPlatY, ...} = Platform.find (foldPlatID, platforms)
             val {y = currentPlatY, id = fromPlatID, ...} = currentPlat
             val newDist = abs (foldPlatY - currentPlatY) + distSoFar
           in
             insertIfNotExistsOrShorter
               (newDist, eKeys, eVals, foldPlatID, q, fromPlatID)
           end
       end)

  (* trace paths for movements:
   * jump + move right, or drop + move right, 
   * jump + move left, drop + move right *)
  structure Horizontal =
    MakeQuadTreeFold
      (struct
         type env = env

         type state = ValSet.elem vector * DistVec.elem vector

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

         fun fold (foldPlatID, env: env, (eVals, q)) =
           let
             val {platforms, currentPlat, eKeys, distSoFar} = env

             val foldPlat = Platform.find (foldPlatID, platforms)
             val foldPlatY = #y foldPlat
             val {y = currentPlatY, id = fromPlatID, ...} = currentPlat

             val height = abs (foldPlatY - currentPlatY)
             val width = minWidth (currentPlat, foldPlat)

             val newDist = pythagoras (width, height) + distSoFar
           in
             insertIfNotExistsOrShorter
               (newDist, eKeys, eVals, foldPlatID, q, fromPlatID)
           end
       end)

  fun start (currentPlat: GameType.platform, env: env, state, platformTree) =
    let
      val {x, y, width, ...} = currentPlat

      (* calculate area to search in y axis *)
      val searchY = y - Constants.jumpLimit
      val height = Constants.worldHeight - searchY

      val (eVals, q) = Vertical.foldRegion
        (x, searchY, width, height, env, state, platformTree)
    in
      (eVals, q)
    end
end
