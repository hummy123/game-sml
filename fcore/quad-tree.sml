signature QUAD_TREE =
sig
  type t

  datatype collision_side =
    QUERY_ON_LEFT_SIDE
  | QUERY_ON_TOP_SIDE
  | QUERY_ON_RIGHT_SIDE
  | QUERY_ON_BOTTOM_SIDE

  val insert: int * int * int * int * 
              int * t -> t

  val getCollisions: int * int * int * int * 
                     int * t -> int list

  val helpGetCollisions: int * int * int * int * 
                         int * int list * t 
                         -> int list

  val hasCollisionAt: int * int * int * int *
                      int * t -> bool

  val getItemID: int * int * int * int *
                 t -> int

  val create: int * int -> t
end

structure QuadTree: QUAD_TREE =
struct
  open QuadTreeType

  type item = QuadTreeType.item

  fun create (width, height) =
    LEAF {
      items = Vector.fromList [],
      x = 0,
      y = 0,
      w = width,
      h = height
    }

  fun isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy) =
    ix < cfx andalso
    ifx > cx andalso
    iy < cfy andalso
    ify > cy

  fun isCollidingPlus (ix, iy, iw, ih, cx, cy, cw, ch) =
  let
    val ifx = ix + iw
    val ify = iy + ih
    val cfx = cx + cw
    val cfy = cy + ch
  in
    isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy)
  end

  fun visitTopLeft (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val hw = qW div 2
      val hh = qH div 2

      val ifx = iX + iW
      val ify = iY + iH

      val qmx = qX + hw
      val qmy = qY + hh

      val qfx = qX + qW
      val qfy = qY + qH
    in
      isColliding (iX, iY, ifx, ify, qX, qY, qmx, qmy)
    end

  fun visitTopRight (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val hw = qW div 2
      val hh = qH div 2

      val ifx = iX + iW
      val ify = iY + iH
      
      val qmx = qX + hw
      val qmy = qY + hh

      val qfx = qX + qW
      val qfy = qY + qH
    in
      isColliding (iX, iY, ifx, ify, qmx, qY, qfx, qmy)
    end

  fun visitBottomLeft (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val hw = qW div 2
      val hh = qH div 2

      val ifx = iX + iW
      val ify = iY + iH
      
      val qmx = qX + hw
      val qmy = qY + hh

      val qfx = qX + qW
      val qfy = qY + qH
    in
      isColliding (iX, iY, ifx, ify, qX, qmy, qmx, qfy)
    end

  fun visitBottomRight (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val hw = qW div 2
      val hh = qH div 2

      val ifx = iX + iW
      val ify = iY + iH
      
      val qmx = qX + hw
      val qmy = qY + hh

      val qfx = qX + qW
      val qfy = qY + qH
    in
      isColliding (iX, iY, ifx, ify, qmx, qmy, qfx, qfy)
    end

  fun mkItem (id, startX, startY, width, height) : item =
    { itemID = id
    , startX = startX
    , startY = startY
    , width = width
    , height = height
    }

  type t = QuadTreeType.t

  (* max size of vector before we split it further *)
  val maxSize = 3

  fun mkTopLeft (x, y, w, h, items) =
    let
      val items = Vector.fromList items
      val hw = w div 2
      val hh = h div 2
    in
      LEAF {
        items = items,
        x = x,
        y = y,
        w = hw,
        h = hh
      }
    end

  fun mkTopRight (x, y, w, h, items) =
    let
      val items = Vector.fromList items
      val hw = w div 2
      val hh = h div 2
      val x = x + hw
    in
      LEAF {
      items = items,
      x = x,
      y = y,
      w = hw,
      h = hh
      }
    end

  fun mkBottomLeft (x, y, w, h, items) =
    let
      val items = Vector.fromList items
      val hw = w div 2
      val hh = h div 2
      val y = y + hh
    in
      LEAF {
        items = items,
        x = x,
        y = y,
        w = hw,
        h = hh
      }
    end

  fun mkBottomRight (x, y, w, h, items) =
    let
      val items = Vector.fromList items
      val hw = w div 2
      val hh = h div 2
      val x = x + hw
      val y = y + hh
    in
      LEAF {
        items = items,
        x = x,
        y = y,
        w = hw,
        h = hh
      }
    end

  fun splitLeaf (x, y, w, h, tl: item list, tr: item list, bl: item list, br:
    item list, elements, pos) =
    if pos < 0 then
      let
        val tl = mkTopLeft (x, y, w, h, tl)
        val tr = mkTopRight (x, y, w, h, tr)
        val bl = mkBottomLeft (x, y, w, h, bl)
        val br = mkBottomRight (x, y, w, h, br)
      in
        NODE
          { topLeft = tl
          , topRight = tr
          , bottomLeft = bl
          , bottomRight = br
          , x = x
          , y = y
          , w = w
          , h = h
          }
      end
    else
      let
        val item = Vector.sub (elements, pos)
        val {startX = iX, startY = iY, width = iW, height = iH, ...} = item

        val vtl = visitTopLeft (iX, iY, iW, iH, x, y, w, h)
        val vtr = visitTopRight (iX, iY, iW, iH, x, y, w, h)
        val vbl = visitBottomLeft (iX, iY, iW, iH, x, y, w, h)
        val vbr = visitBottomRight (iX, iY, iW, iH, x, y, w, h)

        val tl = if vtl then item :: tl else tl

        val tr = if vtr then item :: tr else tr

        val bl = if vbl then item :: bl else bl

        val br = if vbr then item :: br else br
      in
        splitLeaf (x, y, w, h, tl, tr, bl, br, elements, pos - 1)
      end

  fun insert (iX, iY, iW, iH, itemID, tree: t) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
      if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
        let
          (* we are not necessarily inserting into all nodes.
           * If isCollidingPlus returns false recursively, 
           * we return the same node back. *)
          val tl = insert (iX, iY, iW, iH, itemID, topLeft)
          val tr = insert (iX, iY, iW, iH, itemID, topRight)
          val bl = insert (iX, iY, iW, iH, itemID, bottomLeft)
          val br = insert (iX, iY, iW, iH, itemID, bottomRight)
        in
          NODE {topLeft = tl, topRight = tr, bottomLeft = bl, bottomRight = br
          , x = x, y = y, w = w, h = h
          }
        end
      else
        tree
    | LEAF {items, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          if Vector.length items + 1 > maxSize then
            (* have to calculate quadrants and split *)
            let
              val pos = Vector.length items - 1
              val item = mkItem (itemID, iX, iY, iW, iH)

              val vtl = visitTopLeft (iX, iY, iW, iH, x, y, w, h)
              val vtr = visitTopRight (iX, iY, iW, iH, x, y, w, h)
              val vbl = visitBottomLeft (iX, iY, iW, iH, x, y, w, h)
              val vbr = visitBottomRight (iX, iY, iW, iH, x, y, w, h)

              val tl = if vtl then [item] else []

              val tr = if vtr then [item] else []

              val bl = if vbl then [item] else []

              val br = if vbr then [item] else []
            in
              splitLeaf
                ( x
                , y
                , w
                , h
                , tl
                , tr
                , bl
                , br
                , items
                , pos
                )
            end
          else
            (* can insert itemID in items vector *)
            let
              val item = mkItem (itemID, iX, iY, iW, iH)
              val items = Vector.concat [items, Vector.fromList [item]]
            in
              LEAF {items = items, x = x, y = y, w = w, h = h}
            end
        else 
          (* bounds of new item don't fit inside leaf so return old tree *)
          tree

  fun isColliding (iX, iY, iW, iH, itemID, checkWith: item) =
    let
      val {itemID = checkID, startX = cX, startY = cY, width = cW, height = cH, ...} = checkWith
    in
      iX < cX + cW andalso
      iX + iW > cX andalso
      iY < cY + cH andalso
      iY + iH > cY andalso 
      itemID <> checkID
    end

  fun getCollisionsVec (iX, iY, iW, iH, itemID, pos, elements, acc) =
    if pos = Vector.length elements then
      acc
    else
      let
        val item = Vector.sub (elements, pos)
        val acc =
          if isColliding (iX, iY, iW, iH, itemID, item) then #itemID item :: acc
          else acc
      in
        getCollisionsVec (iX, iY, iW, iH, itemID, pos + 1, elements, acc)
      end

  fun getCollisionsAll (iX, iY, iW, iH, itemID, acc, tree) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          let
            val acc = getCollisionsAll
              (iX, iY, iW, iH, itemID, acc, topLeft)

            val acc = getCollisionsAll
              (iX, iY, iW, iH, itemID, acc, topRight)

            val acc = getCollisionsAll
              (iX, iY, iW, iH, itemID, acc, bottomLeft)
          in
            getCollisionsAll
              (iX, iY, iW, iH, itemID, acc, bottomRight)
          end
        else
          acc
    | LEAF {items, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          getCollisionsVec (iX, iY, iW, iH, itemID, 0, items, acc)
        else acc

  fun helpGetCollisions
    ( iX
    , iY
    , iW
    , iH
    , itemID
    , acc
    , tree: t
    ) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
      if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
        let
          val acc =
              helpGetCollisions
                (iX, iY, iW, iH, itemID, acc, topLeft)

          val acc =
              helpGetCollisions
                (iX, iY, iW, iH, itemID, acc, topRight)

          val acc =
              helpGetCollisions
                ( iX
                , iY
                , iW
                , iH
                , itemID
                , acc
                , bottomLeft
                )
        in
              helpGetCollisions
                ( iX
                , iY
                , iW
                , iH
                , itemID
                , acc
                , bottomRight
                )
        end
      else
        acc
    | LEAF {items, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          getCollisionsVec
            (iX, iY, iW, iH, itemID, 0, items, acc)
        else
          acc

  fun getCollisions
    ( itemX
    , itemY
    , itemWidth
    , itemHeight
    , itemID
    , tree
    ) =
    helpGetCollisions
      ( itemX
      , itemY
      , itemWidth
      , itemHeight
      , itemID
      , []
      , tree
      )

  (* no variant to represent 'no collision' case
   * because caller should only try getting collision side 
   * after checking that there is any collision. *)
  datatype collision_side =
    QUERY_ON_LEFT_SIDE
  | QUERY_ON_TOP_SIDE
  | QUERY_ON_RIGHT_SIDE
  | QUERY_ON_BOTTOM_SIDE

  (* getCollisionSide function ported from this answer:
   * https://stackoverflow.com/a/56607347
   * *)
  fun getCollisionSide (iX, iY, iW, iH, checkWith: item) =
    let
      val iFinishX = iX + iW
      val iFinishY = iY + iH
      val iHalfW = iW div 2
      val iHalfH = iH div 2
      val iCentreX = iX + iHalfW
      val iCentreY = iY + iHalfH

      val {startX = cX, startY = cY, width = cW, height = cH, ...} = checkWith

      val cFinishX = cX + cW
      val cFinishY = cY + cH
      val cHalfW = cW div 2
      val cHalfH = cH div 2
      val cCentreX = cX + cHalfW
      val cCentreY = cY + cHalfH

      val diffX = iCentreX - cCentreX
      val diffY = iCentreY - cCentreY

      val minXDist = iHalfW + cHalfW
      val minYDist = iHalfH + cHalfH

      val depthX = if diffX > 0 then minXDist - diffX else (~minXDist) - diffX

      val depthY = if diffY > 0 then minYDist - diffY else (~minYDist) - diffY
    in
      if abs depthX < abs depthY then
        if depthX > 0 then QUERY_ON_LEFT_SIDE else QUERY_ON_RIGHT_SIDE
      else if depthY > 0 then
        QUERY_ON_TOP_SIDE
      else
        QUERY_ON_BOTTOM_SIDE
    end

  fun hasCollisionAtVec (iX, iY, iW, iH, itemID, pos, elements) =
    if pos = Vector.length elements then
      false
    else
      let
        val item = Vector.sub (elements, pos)
      in
        isColliding (iX, iY, iW, iH, itemID, item) orelse
        hasCollisionAtVec (iX, iY, iW, iH, itemID, pos + 1, elements)
      end

  fun hasCollisionAt
    ( iX
    , iY
    , iW
    , iH
    , itemID
    , tree
    ) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
                hasCollisionAt
                  (iX, iY, iW, iH, itemID, topLeft)
                  orelse
                hasCollisionAt
                  (iX, iY, iW, iH, itemID, topRight)
                  orelse
                hasCollisionAt
                  (iX, iY, iW, iH, itemID, bottomLeft)
                  orelse
                hasCollisionAt
                  (iX, iY, iW, iH, itemID, bottomRight)
        else 
          false
    | LEAF {items, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          hasCollisionAtVec (iX, iY, iW, iH, itemID, 0, items)
        else false

  fun getItemIDVec (iX, iY, iW, iH, pos, elements) =
    if pos = Vector.length elements then
      ~1
    else
      let
        val item = Vector.sub (elements, pos)
      in
        if isColliding (iX, iY, iW, iH, ~1, item) then #itemID item
        else getItemIDVec (iX, iY, iW, iH, pos + 1, elements)
      end

  fun getItemID (iX, iY, iW, iH, tree) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          let
            val try1 = getItemID (iX, iY, iW, iH, topLeft)
            val try2 = getItemID (iX, iY, iW, iH, topRight)
            val try3 = getItemID (iX, iY, iW, iH, bottomLeft)
            val try4 = getItemID (iX, iY, iW, iH, bottomRight)

            (* get max: we assume query was narrow enough 
             * that only one ID is valid *)
            val a = Int.max (try1, try2)
            val a = Int.max (a, try3)
            val a = Int.max (a, try4)
          in
            a
          end
        else
          ~1
    | LEAF {items, x, y, w, h} => 
        if isCollidingPlus (iX, iY, iW, iH, x, y, w, h) then
          getItemIDVec (iX, iY, iW, iH, 0, items)
        else
          ~1
end
