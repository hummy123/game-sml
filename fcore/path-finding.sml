structure PathFinding =
struct
  (* functor for adding reachable platforms to queue *)
  structure FindReachable =
    MakeQuadFolder
      (struct
         open GameType

         type env =
           { platforms: GameType.platform vector
           , currentPlat: GameType.platform
           , eKeys: IntSet.elem vector
           , distSoFar: int
           }

         type state = ValSet.elem vector * DistHeap.t

         fun isBetween (p1, check, p2) = check >= p1 andalso check <= p2

         fun canJumpUpTo (prevPlat: platform, currentPlat: platform) =
           let
             val {x = prevX, y = prevY, width = prevWidth, ...} = prevPlat
             val {x = curX, y = curY, width = curWidth, ...} = currentPlat

             val prevFinishX = prevX + prevWidth
             val curFinishX = curX + curWidth
           in
             (isBetween (prevX, curX, prevFinishX)
              orelse
              isBetween (prevX, curFinishX, prevFinishX)
              andalso prevY + 155 >= curY)
           end

         fun canDropDownTo (prevPlat: platform, currentPlat: platform) =
           let
             val {x = prevX, y = prevY, width = prevWidth, ...} = prevPlat
             val {x = curX, y = curY, width = curWidth, ...} = currentPlat

             val prevFinishX = prevX + prevWidth
             val curFinishX = curX + curWidth
           in
             (isBetween (prevX, curX, prevFinishX)
              orelse
              isBetween (prevX, curFinishX, prevFinishX) andalso prevY <= curY)
           end

         fun isReachableFromLeft (prevPlat, currentPlat) =
           (* prev = right/from, current = left/to *)
           let
             val {x = prevX, y = prevY, width = prevWidth, ...} = prevPlat
             val {x = curX, y = curY, width = curWidth, ...} = currentPlat

             val enemyX = prevX
             val xDiff = prevX - curX
           in
             if xDiff <= Constants.jumpLimit then
               true
             else
               let
                 val enemyApexX = enemyX - Constants.jumpLimit
                 val enemyApexY = prevY + Constants.jumpLimit

                 val curFinishX = curX + curWidth

                 val diffApexY = enemyApexY - curY
                 val diffApexX = enemyApexX - curFinishX
               in
                 diffApexX <= diffApexY orelse diffApexY <= 0
               end
           end

         fun isReachableFromRight (prevPlat, currentPlat) =
           (* prev = left/from, current = right/to *)
           let
             val {x = prevX, y = prevY, width = prevWidth, ...} = prevPlat
             val {x = curX, y = curY, width = curWidth, ...} = currentPlat

             (* last x coordinate where enemy can fully fit on prevPlat *)
             val enemyX = prevX + prevWidth - Constants.enemySize

             val xDiff = curX - prevX
           in
             if xDiff <= Constants.jumpLimit then
               (* platform is possible to jump to without falling *)
               true
             else
               let
                 val enemyApexX = enemyX + Constants.jumpLimit
                 val enemyApexY = prevY + Constants.jumpLimit

                 val diffApexY = enemyApexY - curY
                 val diffApexX = enemyApexX - curX
               in
                 diffApexY <= 0 orelse diffApexX <= diffApexY
               end
           end

         fun insertIfNotExistsOrShorter
           (dist, eKeys, eVals, foldPlatID, q, fromPlatID) =
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
                     val q =
                       DistHeap.insert
                         ( { distance = dist
                           , id = foldPlatID
                           , comesFrom = fromPlatID
                           }
                         , q
                         )
                   in
                     (eVals, q)
                   end
               end
             else
               (* key not explored, so add to queue *)
               let
                 val q =
                   DistHeap.insert
                     ( { distance = dist
                       , id = foldPlatID
                       , comesFrom = fromPlatID
                       }
                     , q
                     )
               in
                 (eVals, q)
               end
           end

         fun fState ((eVals, q), env, foldPlatID) =
           let
             val {platforms, currentPlat, eKeys, distSoFar} = env
             val curPlatID = #id currentPlat
             val foldPlat = Platform.find (foldPlatID, platforms)
           in
             if
               canJumpUpTo (currentPlat, foldPlat)
               orelse canDropDownTo (currentPlat, foldPlat)
             then
               let
                 (* only need to calculate vertical distance *)
                 val {y = py, ...} = currentPlat
                 val {y = cy, ...} = foldPlat
                 val dist = abs (py - cy)
                 val dist = dist + distSoFar
               in
                 insertIfNotExistsOrShorter
                   (dist, eKeys, eVals, foldPlatID, q, curPlatID)
               end
             else if
               isReachableFromLeft (currentPlat, foldPlat)
               orelse isReachableFromRight (currentPlat, foldPlat)
             then
               let
                 val {x = px, y = py, width = pw, ...} = currentPlat
                 val {x = cx, y = cy, width = cw, ...} = foldPlat

                 val pFinishX = px + pw
                 val cFinishX = cx + cw

                 val dist =
                   if py = cy then
                     let
                       (* if on same y coordinate, 
                        * only need to calculate horizontal distance *)
                       val d1 = abs (px - cx)
                       val d2 = abs (px - cFinishX)
                       val d3 = abs (pFinishX - cx)
                       val d4 = abs (pFinishX - cFinishX)

                       val min = Int.min (d1, d2)
                       val min = Int.min (min, d3)
                     in
                       Int.min (min, d4)
                     end
                   else
                     let
                       (* if they have different y coordinate,
                        * need to calculate diagonal length/hypotenuse by pythagoras
                        * *)
                       val x1 = abs (px - cx)
                       val x2 = abs (px - cFinishX)
                       val x3 = abs (pFinishX - cx)
                       val x4 = abs (pFinishX - cFinishX)

                       val x = Int.min (x1, x2)
                       val x = Int.min (x, x3)
                       val x = Int.min (x, x4)

                       (* there is only one y coordinate for platform
                        * so don't need to 'minimise' it
                        * *)
                       val y = abs (py - cy)

                       (* pythagoras *)
                       val xsq = x * x
                       val ysq = y * y
                       val hypsq = xsq + ysq

                       (* square root to find diagonal length *)
                       val dg = Real.fromInt hypsq
                       val dg = Math.sqrt dg
                     in
                       Real.toInt IEEEReal.TO_NEAREST dg
                     end
                 val dist = dist + distSoFar
               in
                 insertIfNotExistsOrShorter
                   (dist, eKeys, eVals, foldPlatID, q, curPlatID)
               end
             else
               (eVals, q)
           end
       end)

  fun filterMinDuplicates (q, eKeys) =
    let
      val {id = min, ...} = DistHeap.findMin q
      val pos = IntSet.findInsPos (min, eKeys)
    in
      if IntSet.contains (min, eKeys) then
        let val q = DistHeap.deleteMin q
        in filterMinDuplicates (q, eKeys)
        end
      else
        q
    end

  fun helpGetPathList (curID, eID, eKeys, eVals, acc) =
    if curID = eID then
      (* reached starting ID of platform so return *)
      acc
    else
      (* cons curID and traverse links backwards to reconstruct path *)
      let
        val acc = curID :: acc
        val pos = IntSet.findInsPos (curID, eKeys)

        val {from, distance, ...} = ValSet.sub (eVals, pos)
        val _ = print ("266 distance = " ^ Int.toString distance ^ "\n")
      in
        helpGetPathList (from, eID, eKeys, eVals, acc)
      end

  fun getPathList (pID, eID, eKeys, eVals) =
    helpGetPathList (pID, eID, eKeys, eVals, [])

  fun loop (pID, eID, platforms, platformTree, q, eKeys, eVals) =
    let
      (* filtering duplicates because we have no decrease-key operation *)
      val q = filterMinDuplicates (q, eKeys)
    in
      if IntSet.contains (pID, eKeys) then
        (* return path if we explored pid *)
        getPathList (pID, eID, eKeys, eVals)
      else
        (* continue dijkstra's algorithm *)
        let
          val {distance = distSoFar, id = minID, comesFrom} = DistHeap.findMin q
        in
          if minID = ~1 then
            (* return empty list to signify that there is no path *)
            []
          else
            (* find reachable values from min in quad tree *)
            let
              val plat = Platform.find (minID, platforms)

              (* add explored *)
              val insPos = IntSet.findInsPos (minID, eKeys)
              val eKeys = IntSet.insert (eKeys, minID, insPos)
              val eVals =
                ValSet.insert
                  (eVals, {distance = distSoFar, from = comesFrom}, insPos)

              val env =
                { platforms = platforms
                , currentPlat = plat
                , eKeys = eKeys
                , distSoFar = distSoFar
                }

              val state = (eVals, q)

              (* calculate area to fold over quad tree *)
              val ww = Constants.worldWidth
              val wh = Constants.worldHeight

              val {x, y, width, ...} = plat
              val y = y - Constants.jumpLimit
              val height = wh - y

              (* fold over quad tree, updating any distances 
               * we find the shortest path for *)
              val (eVals, q) = FindReachable.foldRegion
                (0, 0, ww, 100, 0, 0, ww, wh, env, state, platformTree)
            in
              loop (pID, eID, platforms, platformTree, q, eKeys, eVals)
            end
        end
    end

  fun start (pID, eID, platforms, platformTree) =
    let
      (* initialise data structures: the priority queue and the explored map *)
      val q = DistHeap.empty
      val q = DistHeap.insert ({distance = 0, id = eID, comesFrom = eID}, q)

      (* explored keys and values *)
      val eKeys = IntSet.empty
      val eVals = ValSet.empty

    (* important: starting node will have a key that points to itself.
     * For example, if starting node is #"e", then the record will say 
     * the "from" field is #"e".
     * This is different from the other nodes, where the "from" field
     * will be the ID of the previous node which led to the current one.
     * This is our terminating condition when reconstructing paths:
     * If the key matching this value is the same as the "from" node,
     * then we're done reconstructing the path and can return the path list.
     * *)
    in
      loop (pID, eID, platforms, platformTree, q, eKeys, eVals)
    end
end
