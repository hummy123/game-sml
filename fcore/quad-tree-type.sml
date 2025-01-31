signature QUAD_TREE_TYPE =
sig
  type item = {itemID: int, startX: int, startY: int, width: int, height: int}

  datatype t =
    NODE of
      { nodes: t vector
      , x: int
      , y: int
      , w: int
      , h: int
      }
  | LEAF of {items: item vector, x: int, y: int, w: int, h: int}

  val tlIdx: int
  val trIdx: int
  val blIdx: int
  val brIdx: int

  val isColliding: int * int * int * int *
                   int * int * int * int
                   -> bool

  val isCollidingPlus: int * int * int * int *
                       int * int * int * int
                       -> bool

  val isCollidingItem: int * int * int * int * 
                       int * item 
                       -> bool

  val visitTopLeft: int * int * int * int *
                    int * int * int * int
                    -> bool

  val visitTopRight: int * int * int * int *
                     int * int * int * int
                     -> bool

  val visitBottomLeft: int * int * int * int *
                       int * int * int * int
                       -> bool

  val visitBottomRight: int * int * int * int *
                        int * int * int * int
                        -> bool
end

structure QuadTreeType :> QUAD_TREE_TYPE =
struct
  type item = {itemID: int, startX: int, startY: int, width: int, height: int}

  datatype t =
    NODE of
      { nodes: t vector
      , x: int
      , y: int
      , w: int
      , h: int
      }
  | LEAF of {items: item vector, x: int, y: int, w: int, h: int}

  val tlIdx = 0
  val trIdx = 1
  val blIdx = 2
  val brIdx = 3

  fun isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy) =
    ix < cfx andalso ifx > cx andalso iy < cfy andalso ify > cy

  fun isCollidingPlus (ix, iy, iw, ih, cx, cy, cw, ch) =
    let
      val ifx = ix + iw
      val ify = iy + ih
      val cfx = cx + cw
      val cfy = cy + ch
    in
      isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy)
    end

  fun isCollidingItem (iX, iY, iW, iH, itemID, checkWith: item) =
    let
      val
        { itemID = checkID
        , startX = cX
        , startY = cY
        , width = cW
        , height = cH
        , ...
        } = checkWith
    in
      isCollidingPlus (iX, iY, iW, iH, cX, cY, cW, cH) andalso itemID <> checkID
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
end
