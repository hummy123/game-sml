structure QuadTree =
struct
  type item = {itemID: int, startX: int, startY: int, width: int, height: int}

  fun mkItem (id, startX, startY, width, height) : item =
    { itemID = id
    , startX = startX
    , startY = startY
    , width = width
    , height = height
    }

  datatype t =
    NODE of
      { topLeft: t
      , topRight: t
      , bottomLeft: t
      , bottomRight: t
      , elements: item vector
      }
  | LEAF of item vector

  (* max size of vector before we split it further *)
  val maxSize = 3

  fun isItemInQuad (iX, iY, iWidth, iHeight, qX, qY, qWidth, qHeight) =
    iX >= qX andalso iY >= qY andalso iWidth <= qWidth
    andalso iHeight <= qHeight

  datatype quadrant =
    TOP_LEFT
  | TOP_RIGHT
  | BOTTOM_LEFT
  | BOTTOM_RIGHT
  | PARENT_QUADRANT

  fun whichQuadrant
    (itemX, itemY, itemWidth, itemHeight, quadX, quadY, quadWidth, quadHeight) =
    let
      (* calculate quadrants *)
      val halfWidth = quadWidth div 2
      val halfHeight = quadHeight div 2

      val middleX = quadX + halfWidth
      val middleY = quadY + halfHeight

      val isInTopLeft = isItemInQuad
        ( itemX, itemY, itemWidth, itemHeight
        , quadX, quadY, halfWidth, halfHeight
        )

      val isInTopRight = isItemInQuad
        ( itemX, itemY, itemWidth, itemHeight
        , middleX, quadY, halfWidth, halfHeight
        )

      val isInBottomLeft = isItemInQuad
        ( itemX, itemY, itemWidth, itemHeight
        , quadX, middleY, halfWidth, halfHeight
        )

      val isInBottomRight = isItemInQuad
        ( itemX, itemY, itemWidth, itemHeight
        , middleX, middleY, halfWidth, halfHeight
        )
    in
      if isInTopLeft then TOP_LEFT
      else if isInTopRight then TOP_RIGHT
      else if isInBottomLeft then BOTTOM_LEFT
      else if isInBottomRight then BOTTOM_RIGHT
      else PARENT_QUADRANT
    end

  fun splitLeaf (qX, qY, qW, qH, tl, tr, bl, br, pe, elements, pos) =
    if pos < 0 then
      let
        val tl = Vector.fromList tl
        val tr = Vector.fromList tr
        val bl = Vector.fromList bl
        val br = Vector.fromList br
        val pe = Vector.fromList pe
      in
        NODE
          { topLeft = LEAF tl
          , topRight = LEAF tr
          , bottomLeft = LEAF bl
          , bottomRight = LEAF br
          , elements = pe
          }
      end
    else
      let
        val item = Vector.sub (elements, pos)
        val {startX = iX, startY = iY, width = iW, height = iH, ...} = item
      in
        case whichQuadrant (iX, iY, iW, iH, qX, qY, qW, qH) of
          TOP_LEFT =>
            splitLeaf
              (qX, qY, qW, qH, item :: tl, tr, bl, br, pe, elements, pos - 1)
        | TOP_RIGHT =>
            splitLeaf
              (qX, qY, qW, qH, tl, item :: tr, bl, br, pe, elements, pos - 1)
        | BOTTOM_LEFT =>
            splitLeaf
              (qX, qY, qW, qH, tl, tr, item :: bl, br, pe, elements, pos - 1)
        | BOTTOM_RIGHT =>
            splitLeaf
              (qX, qY, qW, qH, tl, tr, bl, item :: br, pe, elements, pos - 1)
        | PARENT_QUADRANT =>
            splitLeaf
              (qX, qY, qW, qH, tl, tr, bl, br, item :: pe, elements, pos - 1)
      end

  fun insert
    ( itemX, itemY, itemWidth, itemHeight
    , quadX, quadY, quadWidth, quadHeight
    , itemID, tree: t
    ) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, elements} =>
        (* check which quadrant item is in, if any.
         * If in any child quadrants, recurse insertion into there.
         * Else, add to elements vector in current node. *)
        (case
           whichQuadrant
             ( itemX, itemY, itemWidth, itemHeight
             , quadX, quadY, quadWidth, quadHeight
             )
         of
           TOP_LEFT =>
             let
               (* I know I am repeating myself by recalculating 
                * halfWidth/halfHeight in case branches but I prefer this 
                * over increating the indentation level further
                * *)
               val halfWidth = quadWidth div 2
               val halfHeight = quadHeight div 2

               val newTopLeft = insert
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX, quadY, halfWidth, halfHeight
                 , itemID, topLeft
                 )
             in
               NODE
                 { topLeft = newTopLeft
                 , topRight = topRight
                 , bottomLeft = bottomLeft
                 , bottomRight = bottomRight
                 , elements = elements
                 }
             end
         | TOP_RIGHT =>
             let
               val halfWidth = quadWidth div 2
               val halfHeight = quadHeight div 2
               val middleX = quadX + halfWidth

               val newTopRight = insert
                 ( itemX, itemY, itemWidth, itemHeight
                 , middleX, quadY, halfWidth, halfHeight
                 , itemID, topRight
                 )
             in
               NODE
                 { topLeft = topLeft
                 , topRight = newTopRight
                 , bottomLeft = bottomLeft
                 , bottomRight = bottomRight
                 , elements = elements
                 }
             end
         | BOTTOM_LEFT =>
             let
               val halfWidth = quadWidth div 2
               val halfHeight = quadHeight div 2
               val middleY = quadY + halfHeight

               val newBottomLeft = insert
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX, middleY, halfWidth, halfHeight
                 , itemID, bottomLeft
                 )
             in
               NODE
                 { topLeft = topLeft
                 , topRight = topRight
                 , bottomLeft = newBottomLeft
                 , bottomRight = bottomRight
                 , elements = elements
                 }
             end
         | BOTTOM_RIGHT =>
             let
               val halfWidth = quadWidth div 2
               val halfHeight = quadHeight div 2
               val middleX = quadX + halfWidth
               val middleY = quadY + halfHeight

               val newBottomRight = insert
                 ( itemX, itemY, itemWidth, itemHeight
                 , middleX, middleY, halfWidth, halfHeight
                 , itemID, bottomRight
                 )
             in
               NODE
                 { topLeft = topLeft
                 , topRight = topRight
                 , bottomLeft = bottomLeft
                 , bottomRight = newBottomRight
                 , elements = elements
                 }
             end
         | PARENT_QUADRANT =>
             (* Does not fit in any of the child quadrants
              * so we must add to the current parent quadrant. *)
             let
               val item = mkItem (itemID, itemX, itemY, itemWidth, itemHeight)
               val elements = Vector.concat [elements, Vector.fromList [item]]
             in
               NODE
                 { topLeft = topLeft
                 , topRight = topRight
                 , bottomLeft = bottomLeft
                 , bottomRight = bottomRight
                 , elements = elements
                 }
             end)
    | LEAF elements =>
        if Vector.length elements + 1 > maxSize then
          (* have to calculate quadrants and split *)
          let
            val pos = Vector.length elements - 1
            val item = mkItem (itemID, itemX, itemY, itemWidth, itemHeight)
          in
            (case
               whichQuadrant
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX, quadY, quadWidth, quadHeight
                 )
             of
               TOP_LEFT =>
                 splitLeaf
                   ( quadX, quadY, quadWidth, quadHeight
                   , [item], [], [], [], []
                   , elements, pos
                   )
             | TOP_RIGHT =>
                 splitLeaf
                   ( quadX, quadY, quadWidth, quadHeight
                   , [], [item], [], [], []
                   , elements, pos
                   )
             | BOTTOM_LEFT =>
                 splitLeaf
                   ( quadX, quadY, quadWidth, quadHeight
                   , [], [], [item], [], []
                   , elements, pos
                   )
             | BOTTOM_RIGHT =>
                 splitLeaf
                   ( quadX, quadY, quadWidth, quadHeight
                   , [], [], [], [item], []
                   , elements, pos
                   )
             | PARENT_QUADRANT =>
                 splitLeaf
                   ( quadX, quadY, quadWidth, quadHeight
                   , [], [], [], [], [item]
                   , elements, pos
                   ))
          end
        else
          (* can insert itemID in elements vector *)
          let
            val item = mkItem (itemID, itemX, itemY, itemWidth, itemHeight)
            val elements = Vector.concat [elements, Vector.fromList [item]]
          in
            LEAF elements
          end

  fun isColliding (iX, iY, iW, iH, itemID, checkWith: item) =
    let
      val itemEndX = iX + iW
      val itemEndY = iY + iH
      val {itemID = checkID, startX, startY, width, height, ...} = checkWith
      val endX = startX + width
      val endY = startY + height
    in
      iX < endX 
      andalso itemEndX > startX
      andalso iY < endY 
      andalso itemEndY > startY
      andalso itemID <> checkID
    end

  fun getCollisionsVec (iX, iY, iW, iH, itemID, pos, elements, acc) =
    if pos = Vector.length elements then
      acc
    else
      let
        val item = Vector.sub (elements, pos)
        val acc = 
          if isColliding (iX, iY, iW, iH, itemID, item)
          then item :: acc
          else acc
      in
        getCollisionsVec (iX, iY, iW, iH, itemID, pos + 1, elements, acc)
      end

  fun getCollisionsAll 
    ( iX, iY, iW, iH, qW, qH
    , itemID, acc, tree
    ) =
    case tree of 
      NODE {topLeft, topRight, bottomLeft, bottomRight, elements} =>
        let
          val acc = 
            getCollisionsVec (iX, iY, iW, iH, itemID, 0, elements, acc)
          val halfWidth = qW div 2
          val halfHeight = qH div 2

          val acc = 
            getCollisionsAll
              ( iX, iY, iW, iH, halfWidth, halfHeight
              , itemID, acc, topLeft
              )

          val acc = 
            getCollisionsAll
              ( iX, iY, iW, iH, halfWidth, halfHeight
              , itemID, acc, topRight
              )

          val acc = 
            getCollisionsAll
              ( iX, iY, iW, iH, halfWidth, halfHeight
              , itemID, acc, bottomLeft
              )
        in
          getCollisionsAll
            ( iX, iY, iW, iH, halfWidth, halfWidth
            , itemID, acc, bottomRight
            )
        end
    | LEAF elements =>
        getCollisionsVec (iX, iY, iW, iH, itemID, 0, elements, acc)

  fun getCollisions 
    ( itemX, itemY, itemWidth, itemHeight
    , quadX, quadY, quadWidth, quadHeight
    , itemID, acc, tree: t
    ) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, elements} =>
        let
          (* get colliding elements in this node first *)
          val acc = 
            getCollisionsVec 
              ( itemX, itemY, itemWidth, itemHeight
              , itemID, 0, elements, acc
              )
          val halfWidth = quadWidth div 2
          val halfHeight = quadHeight div 2
        in
          (case whichQuadrant 
            ( itemX, itemY, itemWidth, itemHeight
            , quadX, quadY, quadWidth, quadHeight
            ) 
           of
             TOP_LEFT =>
               getCollisions 
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX, quadY, halfWidth, halfHeight
                 , itemID, acc, topLeft
                 )
           | TOP_RIGHT =>
               getCollisions 
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX + halfWidth, quadY, halfWidth, halfHeight
                 , itemID, acc, topRight
                 )
           | BOTTOM_LEFT =>
               getCollisions 
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX, quadY + halfHeight, halfWidth, halfHeight
                 , itemID, acc, bottomLeft
                 )
           | BOTTOM_RIGHT => 
               getCollisions 
                 ( itemX, itemY, itemWidth, itemHeight
                 , quadX + halfWidth, quadY + halfHeight
                 , halfWidth, halfHeight
                 , itemID, acc, bottomRight
                 )
           | PARENT_QUADRANT => 
               (* In this function, PARENT_QUADRANT means 
                * that the item is not in any of the main quadrants 
                * but may possibly in the parent quadrant OR 
                * it may be in any of the child quadrants. 
                * So descend down on all the children, accumulating acc. 
                * *)
                let
                  val acc = 
                    getCollisionsAll
                      ( itemX, itemY, itemWidth, itemHeight
                      , halfWidth, halfHeight
                      , itemID, acc, topLeft
                      )

                  val acc = 
                    getCollisionsAll
                      ( itemX, itemY, itemWidth, itemHeight
                      , halfWidth, halfHeight
                      , itemID, acc, topRight
                      )

                  val acc = 
                    getCollisionsAll
                      ( itemX, itemY, itemWidth, itemHeight
                      , halfWidth, halfHeight
                      , itemID, acc, bottomLeft
                      )
                in
                  getCollisionsAll
                    ( itemX, itemY, itemWidth, itemHeight
                    , halfWidth, halfHeight
                    , itemID, acc, bottomRight
                    )
                end)
        end
    | LEAF elements =>
        getCollisionsVec 
          ( itemX, itemY, itemWidth, itemHeight
          , itemID, 0, elements, acc
          )

  datatype t =
    NODE of
      { topLeft: t
      , topRight: t
      , bottomLeft: t
      , bottomRight: t
      , elements: item vector
      }
  | LEAF of item vector
end
