structure PointerQuadTree =
struct
  open PointerQuadTreeType

  type item = PointerQuadTreeType.item

  type t = {width: int, height: int, tree: PointerQuadTreeType.t}

  fun mkItem (id, startX, startY, width, height) : item =
    { itemID = id
    , startX = startX
    , startY = startY
    , width = width
    , height = height
    }

  fun create (width, height) = {width = width, height = height, tree = EMPTY}

  fun hasSameCoordinates (prev: item, new: item) =
    let
      val {startX = px, startY = py, width = pw, height = ph, ...} = prev
      val {startX = nx, startY = ny, width = nw, height = nh, ...} = new
    in
      px = nx andalso py = ny andalso pw = nw andalso ph = nh
    end

  fun mkLeaf (visitPrev, visitNew, prevItem, newItem) =
    if visitPrev then LEAF prevItem
    else if visitNew then LEAF newItem
    else EMPTY

  fun helpSplitLeaf (x, y, w, h, prevItem, newItem) =
    let
      val {startX = px, startY = py, width = pw, height = ph, ...} = prevItem
      val {startX = nx, startY = ny, width = nw, height = nh, ...} = newItem

      val vtlPrev = visitTopLeft (px, py, pw, ph, x, y, w, h)
      val vtrPrev = visitTopRight (px, py, pw, ph, x, y, w, h)
      val vblPrev = visitBottomLeft (px, py, pw, ph, x, y, w, h)
      val vbrPrev = visitBottomRight (px, py, pw, ph, x, y, w, h)

      val vtlNew = visitTopLeft (nx, ny, nw, nh, x, y, w, h)
      val vtrNew = visitTopRight (nx, ny, nw, nh, x, y, w, h)
      val vblNew = visitBottomLeft (nx, ny, nw, nh, x, y, w, h)
      val vbrNew = visitBottomRight (nx, ny, nw, nh, x, y, w, h)

      val hw = w div 2
      val hh = h div 2
      val mx = x + hw
      val my = y + hh

      val tl =
        if vtlPrev andalso vtlNew then
          helpSplitLeaf (x, y, hw, hh, prevItem, newItem)
        else
          mkLeaf (vtlPrev, vtlNew, prevItem, newItem)

      val tr =
        if vtrPrev andalso vtrNew then
          helpSplitLeaf (mx, y, hw, hh, prevItem, newItem)
        else
          mkLeaf (vtrPrev, vtrNew, prevItem, newItem)

      val bl =
        if vblPrev andalso vblNew then
          helpSplitLeaf (x, my, hw, hh, prevItem, newItem)
        else
          mkLeaf (vblPrev, vblNew, prevItem, newItem)

      val br =
        if vbrPrev andalso vbrNew then
          helpSplitLeaf (mx, my, hw, hh, prevItem, newItem)
        else
          mkLeaf (vbrPrev, vbrNew, prevItem, newItem)
    in
      NODE {tl = tl, tr = tr, bl = bl, br = br}
    end

  fun splitLeaf (x, y, w, h, prevItem, newItem) =
    if hasSameCoordinates (prevItem, newItem) then
      SHARE_LEAF [prevItem, newItem]
    else
      helpSplitLeaf (x, y, w, h, prevItem, newItem)

  fun mkShareLeaf (visitPrev, visitNew, oldItems, newItem) =
    if visitPrev then SHARE_LEAF oldItems
    else if visitNew then LEAF newItem
    else EMPTY

  fun helpSplitShareLeaf (x, y, w, h, oldItems, prevItem, newItem) =
    let
      val {startX = px, startY = py, width = pw, height = ph, ...} = prevItem
      val {startX = nx, startY = ny, width = nw, height = nh, ...} = newItem

      val vtlPrev = visitTopLeft (px, py, pw, ph, x, y, w, h)
      val vtrPrev = visitTopRight (px, py, pw, ph, x, y, w, h)
      val vblPrev = visitBottomLeft (px, py, pw, ph, x, y, w, h)
      val vbrPrev = visitBottomRight (px, py, pw, ph, x, y, w, h)

      val vtlNew = visitTopLeft (nx, ny, nw, nh, x, y, w, h)
      val vtrNew = visitTopRight (nx, ny, nw, nh, x, y, w, h)
      val vblNew = visitBottomLeft (nx, ny, nw, nh, x, y, w, h)
      val vbrNew = visitBottomRight (nx, ny, nw, nh, x, y, w, h)

      val hw = w div 2
      val hh = h div 2
      val mx = x + hw
      val my = y + hh

      val tl =
        if vtlPrev andalso vtlNew then
          helpSplitShareLeaf (x, y, hw, hh, oldItems, prevItem, newItem)
        else
          mkShareLeaf (vtlPrev, vtlNew, oldItems, newItem)

      val tr =
        if vtrPrev andalso vtrNew then
          helpSplitShareLeaf (mx, y, hw, hh, oldItems, prevItem, newItem)
        else
          mkShareLeaf (vtrPrev, vtrNew, oldItems, newItem)

      val bl =
        if vblPrev andalso vblNew then
          helpSplitShareLeaf (x, my, hw, hh, oldItems, prevItem, newItem)
        else
          mkShareLeaf (vblPrev, vblNew, oldItems, newItem)

      val br =
        if vbrPrev andalso vbrNew then
          helpSplitShareLeaf (mx, my, hw, hh, oldItems, prevItem, newItem)
        else
          mkShareLeaf (vbrPrev, vbrNew, oldItems, newItem)
    in
      NODE {tl = tl, tr = tr, bl = bl, br = br}
    end

  fun splitShareLeaf (x, y, w, h, oldItems, newItem) =
    case oldItems of
      prevItem :: tl =>
        if hasSameCoordinates (prevItem, newItem) then
          let val newItems = newItem :: oldItems
          in SHARE_LEAF newItems
          end
        else
          helpSplitShareLeaf (x, y, w, h, oldItems, prevItem, newItem)
    | [] => 
        (* this case never occurs *) 
        LEAF newItem

  fun helpInsert (ix, iy, iw, ih, itemID, qx, qy, qw, qh, tree) =
    case tree of
      NODE {tl, tr, bl, br} =>
        let
          val vtl = visitTopLeft (ix, iy, iw, ih, qx, qy, qw, qh)
          val vtr = visitTopRight (ix, iy, iw, ih, qx, qy, qw, qh)
          val vbl = visitBottomLeft (ix, iy, iw, ih, qx, qy, qw, qh)
          val vbr = visitBottomRight (ix, iy, iw, ih, qx, qy, qw, qh)

          val hw = qw div 2
          val hh = qh div 2

          val tl =
            if vtl then helpInsert (ix, iy, iw, ih, itemID, qw, qy, hw, hh, tl)
            else tl

          val tr =
            if vtr then
              helpInsert (ix, iy, iw, ih, itemID, qx + hw, qy, hw, hh, tr)
            else
              tr

          val bl =
            if vbl then
              helpInsert (ix, iy, iw, ih, itemID, qx, qy + hh, hw, hh, bl)
            else
              bl

          val br =
            if vbr then
              helpInsert (ix, iy, iw, ih, itemID, qx + hw, qy + hh, hw, hh, br)
            else
              br
        in
          NODE {tl = tl, tr = tr, bl = bl, br = br}
        end
    | LEAF prevItem =>
        let val newItem = mkItem (itemID, ix, iy, iw, ih)
        in splitLeaf (qx, qy, qw, qh, prevItem, newItem)
        end
    | EMPTY =>
        let val newItem = mkItem (itemID, ix, iy, iw, ih)
        in LEAF newItem
        end
    | SHARE_LEAF oldItems =>
        let val newItem = mkItem (itemID, ix, iy, iw, ih)
        in splitShareLeaf (qx, qy, qw, qh, oldItems, newItem)
        end
end
