structure PathFinding =
struct
  fun filterMinDuplicates (q, eKeys) =
    let
      val min = DistHeap.findMin q
    in
      if IntSet.contains (min, eKeys) then
        let
          val q = DistHeap.deleteMin q
        in
          filterMinDuplicates (q, eKeys)
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
        val {from, ...} = IntSet.sub (eVals, pos)
      in
        helpGetPathList (from, eID, eKeys, eVals, acc)
      end

  fun getPathList (pID, eID, eKeys, eVals) =
    helpGetPathList (pID, eID, eKeys, eVals, [])

  fun loop (pID, eID, platforms, platformTree, q, eKeys, eVals) =
    let
      (* filtering duplicates because we have no decrease-key operation *)
      val q = filterMinDuplicates (q, eKeys)
      val min = DistHeap.findMin q
    in
      if min = ~1 then
        (* return empty list to signify that there is no path *)
        []
      else if min = pID then
        (* found path to destination so reconstruct path and return *)
        getPathList (pID, eID, eKeys, eVals)
      else
        (* find reachable values from min in quad tree *)
        let
          val plat = Platform.find (min, platforms)

  fun start (pID, eID, platforms, platformTree) =
    let
      (* initialise data structures: the priority queue and the explored map *)
      val q = DistHeap.empty
      val q = DistHeap.insert ({distance = 0, id = eID}, q)

      val exploredKeys = IntSet.empty
      val exploredVals = ValSet.empty

      val insPos = IntSet.findInsPos (eID, exploredKeys)
      val exploredKeys = IntSet.insert (exploredKeys, eID, insPos)

      (* important: starting node will have a key that points to itself.
       * For example, if starting node is #"e", then the record will say 
       * the "from" field is #"e".
       * This is different from the other nodes, where the "from" field
       * will be the ID of the previous node which led to the current one.
       * This is our terminating condition when reconstructing paths:
       * If the key matching this value is the same as the "from" node,
       * then we're done reconstructing the path and can return the path list.
       * *)
      val eVal = {distance = 0, from = Char.fromInt eID}
      val exploredVals = ValSet.insert (exploredVals, eVal, insPos)
    in
      loop (pID, eID, platforms, platformTree, q, eKeys, eVals)
    end
end
