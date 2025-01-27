signature QUAD_TREE_TYPE =
sig
  type item = {itemID: int, startX: int, startY: int, width: int, height: int}

  datatype t =
    NODE of {topLeft: t, topRight: t, bottomLeft: t, bottomRight: t}
  | LEAF of item vector

  datatype quadrant =
    TOP_LEFT
  | TOP_RIGHT
  | BOTTOM_LEFT
  | BOTTOM_RIGHT
  | PARENT_QUADRANT
end

structure QuadTreeType :> QUAD_TREE_TYPE =
struct
  type item = {itemID: int, startX: int, startY: int, width: int, height: int}

  datatype t =
    NODE of {topLeft: t, topRight: t, bottomLeft: t, bottomRight: t}
  | LEAF of item vector

  datatype quadrant =
    TOP_LEFT
  | TOP_RIGHT
  | BOTTOM_LEFT
  | BOTTOM_RIGHT
  | PARENT_QUADRANT
end
