signature QUAD_TREE =
sig
  type t = {tree: QuadTreeType.t, width: int, height: int}

  val insert: int * int * int * int * int * t -> t

  val getCollisions: int * int * int * int * int * t -> int list

  val hasCollisionAt: int * int * int * int * int * t -> bool

  val getItemID: int * int * int * int * t -> int

  val create: int * int -> t
end

structure QuadTree: QUAD_TREE =
struct
  open QuadTreeType

  type item = QuadTreeType.item

  type t = {tree: QuadTreeType.t, width: int, height: int}

  fun create (width, height) =
    let
      val vec = Vector.fromList []
      val tree = LEAF vec
    in
      {tree = tree, width = width, height = height}
    end

  fun mkItem (id, startX, startY, width, height) : item =
    { itemID = id
    , startX = startX
    , startY = startY
    , width = width
    , height = height
    }

  (* max size of vector before we split it further *)
  val maxSize = 16

  fun mkLeaf items =
    let val items = Vector.fromList items
    in LEAF items
    end

  fun splitLeaf
    ( x
    , y
    , w
    , h
    , tl: item list
    , tr: item list
    , bl: item list
    , br: item list
    , elements
    , pos
    ) =
    if pos < 0 then
      let
        val tl = mkLeaf tl
        val tr = mkLeaf tr
        val bl = mkLeaf bl
        val br = mkLeaf br
        val nodes = Vector.fromList [tl, tr, bl, br]
      in
        NODE nodes
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

  fun helpInsert (ix, iy, iw, ih, itemID, qx, qy, qw, qh, tree) =
    case tree of
      NODE nodes =>
        let
          val vtl = visitTopLeft (ix, iy, iw, ih, qx, qy, qw, qh)
          val vtr = visitTopRight (ix, iy, iw, ih, qx, qy, qw, qh)
          val vbl = visitBottomLeft (ix, iy, iw, ih, qx, qy, qw, qh)
          val vbr = visitBottomRight (ix, iy, iw, ih, qx, qy, qw, qh)

          val hw = qw div 2
          val hh = qh div 2

          val tl = Vector.sub (nodes, tlIdx)
          val tl =
            if vtl then helpInsert (ix, iy, iw, ih, itemID, qw, qy, hw, hh, tl)
            else tl

          val tr = Vector.sub (nodes, trIdx)
          val tr =
            if vtr then
              helpInsert (ix, iy, iw, ih, itemID, qx + hw, qy, hw, hh, tr)
            else
              tr

          val bl = Vector.sub (nodes, blIdx)
          val bl =
            if vbl then
              helpInsert (ix, iy, iw, ih, itemID, qx, qy + hh, hw, hh, bl)
            else
              bl

          val br = Vector.sub (nodes, brIdx)
          val br =
            if vbr then
              helpInsert (ix, iy, iw, ih, itemID, qx + hw, qy + hh, hw, hh, br)
            else
              br

          val nodes = Vector.fromList [tl, tr, bl, br]
        in
          NODE nodes
        end
    | LEAF items =>
        if Vector.length items + 1 > maxSize then
          let
            val vtl = visitTopLeft (ix, iy, iw, ih, qx, qy, qw, qh)
            val vtr = visitTopRight (ix, iy, iw, ih, qx, qy, qw, qh)
            val vbl = visitBottomLeft (ix, iy, iw, ih, qx, qy, qw, qh)
            val vbr = visitBottomRight (ix, iy, iw, ih, qx, qy, qw, qh)

            val newItem = mkItem (itemID, ix, iy, iw, ih)

            val tl = if vtl then [newItem] else []
            val tr = if vtr then [newItem] else []
            val bl = if vbl then [newItem] else []
            val br = if vbr then [newItem] else []
          in
            splitLeaf
              (qx, qy, qw, qh, tl, tr, bl, br, items, Vector.length items - 1)
          end
        else
          let
            val newItem = mkItem (itemID, ix, iy, iw, ih)
            val newItems = Vector.concat [items, Vector.fromList [newItem]]
          in
            LEAF newItems
          end

  fun insert (iX, iY, iW, iH, itemID, tree: t) =
    let
      val {width, height, tree} = tree
      val tree = helpInsert (iX, iY, iW, iH, itemID, 0, 0, width, height, tree)
    in
      {width = width, height = height, tree = tree}
    end

  structure GetCollisions =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = int list
         fun fold (itemID, (), lst) = itemID :: lst
       end)

  fun getCollisions (itemX, itemY, itemWidth, itemHeight, _, tree) =
    GetCollisions.foldRegion (itemX, itemY, itemWidth, itemHeight, (), [], tree)

  structure HasCollisionAt =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = bool
         fun fold _ = true
       end)

  fun hasCollisionAt (ix, iy, iw, ih, _, tree) =
    HasCollisionAt.foldRegion (ix, iy, iw, ih, (), false, tree)

  structure GetItemID =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = int
         fun fold (itemID, (), curID) = Int.max (itemID, curID)
       end)

  fun getItemID (ix, iy, iw, ih, tree) =
    GetItemID.foldRegion (ix, iy, iw, ih, (), ~1, tree)
end
