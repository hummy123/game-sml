structure PathFinding =
struct
  fun filterMinDuplicates (q, eKeys) =
    if DistVec.isEmpty q then
      q
    else
      let
        val {id = min, ...} = DistVec.findMin q

        val pos = IntSet.findInsPos (min, eKeys)
      in
        if IntSet.contains (min, eKeys) then
          let val q = DistVec.deleteMin q
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
      in
        helpGetPathList (from, eID, eKeys, eVals, acc)
      end

  fun getPathList (pID, eID, eKeys, eVals) =
    helpGetPathList (pID, eID, eKeys, eVals, [])

  fun helpExplore (pos, graphNode, minID, distSoFar, q) =
    if pos = Vector.length graphNode then
      q
    else
      let
        val {distance = newPlatDist, id = newPlatID} =
          Vector.sub (graphNode, pos)

        val totalDist = newPlatDist + distSoFar

        val insRecord =
          {distance = totalDist, id = newPlatID, comesFrom = minID}
        val insPos = DistVec.findInsPos (insRecord, q)
        val q = DistVec.insert (q, insRecord, insPos)
      in
        helpExplore (pos + 1, graphNode, minID, distSoFar, q)
      end

  fun explore (graphNode, minID, distSoFar, q) =
    helpExplore (0, graphNode, minID, distSoFar, q)

  fun loop (pID, eID, platforms, platformTree, q, eKeys, eVals, graph) =
    if IntSet.contains (pID, eKeys) then
      (* return path if we explored pid *)
      getPathList (pID, eID, eKeys, eVals)
    else
      (* continue dijkstra's algorithm *)
      let
        (* filtering duplicates because we have no decrease-key operation *)
        val q = filterMinDuplicates (q, eKeys)
      in
        if DistVec.isEmpty q then
          (* return empty list to signify that there is no path *)
          []
        else
          (* find reachable values from min in quad tree *)
          let
            val {distance = distSoFar, id = minID, comesFrom} =
              DistVec.findMin q

            (* explore platforms connected to minID *)
            val platPos = Platform.findPos (minID, platforms)
            val plat = Vector.sub (platforms, platPos)
            val graphNode = Vector.sub (graph, platPos)

            (* on each loop, increment distSoFar by 15.
             * Result: paths that require jumps to fewer platforms are
             * incentivised a little bit. *)
            val q = explore (graphNode, minID, distSoFar + 15, q)

            (* mark platform with (id = minID) as explored *)
            val insPos = IntSet.findInsPos (minID, eKeys)
            val eKeys = IntSet.insert (eKeys, minID, insPos)
            val eVals =
              ValSet.insert
                (eVals, {distance = distSoFar, from = comesFrom}, insPos)
          in
            loop (pID, eID, platforms, platformTree, q, eKeys, eVals, graph)
          end
      end

  (* dead loop function: remove after adding graph to game type 
   * and moving to other loop *)
  fun loop (pID, eID, platforms, platformTree, q, eKeys, eVals) =
    if IntSet.contains (pID, eKeys) then
      (* return path if we explored pid *)
      getPathList (pID, eID, eKeys, eVals)
    else
      (* continue dijkstra's algorithm *)
      let
        (* filtering duplicates because we have no decrease-key operation *)
        val q = filterMinDuplicates (q, eKeys)
      in
        if DistVec.isEmpty q then
          (* return empty list to signify that there is no path *)
          []
        else
          (* find reachable values from min in quad tree *)
          let
            val {distance = distSoFar, id = minID, comesFrom} =
              DistVec.findMin q
            val plat = Platform.find (minID, platforms)

            (* add explored *)
            val insPos = IntSet.findInsPos (minID, eKeys)
            val eKeys = IntSet.insert (eKeys, minID, insPos)
            val eVals =
              ValSet.insert
                (eVals, {distance = distSoFar, from = comesFrom}, insPos)

            (* on each loop, increment distSoFar by 15.
             * Result: paths that require jumps to fewer platforms are
             * incentivised a little bit. *)
            val env =
              { platforms = platforms
              , currentPlat = plat
              , eKeys = eKeys
              , distSoFar = distSoFar + 15
              }

            (* fold over quad tree, updating any distances 
             * we find the shortest path for *)
            val (eVals, q) =
              BuildGraph.start (plat, env, (eVals, q), platformTree)
          in
            loop (pID, eID, platforms, platformTree, q, eKeys, eVals)
          end
      end

  fun start (pID, eID, platforms, platformTree) =
    let
      (* initialise data structures: the priority queue and the explored map *)
      val q = DistVec.fromList [{distance = 0, id = eID, comesFrom = eID}]

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
