signature QUAD_FOLDER =
sig
  type env
  type state

  val fold: int * env * state -> state
end

signature MAKE_QUAD_TREE_FOLD =
sig
  structure Fn: QUAD_FOLDER

  val foldRegion: int * int * int * int * 
                  Fn.env * Fn.state * QuadTreeType.t
                  -> Fn.state
end

functor MakeQuadTreeFold(Fn: QUAD_FOLDER): MAKE_QUAD_TREE_FOLD =
struct
  structure Fn = Fn

  open QuadTreeType

  fun foldRegionVec (rx, ry, rw, rh, env, state, pos, elements) =
    if pos = Vector.length elements then
      state
    else
      let
        val item = Vector.sub (elements, pos)
        val state =
          if isCollidingItem (rx, ry, rw, rh, ~1, item) then
            Fn.fold (#itemID item, env, state)
          else
            state
      in
        foldRegionVec (rx, ry, rw, rh, env, state, pos + 1, elements)
      end

  fun foldRegion (rx, ry, rw, rh, env, state, tree) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, x, y, w, h} =>
        if isCollidingPlus (rx, ry, rw, rh, x, y, w, h) then
          let
            val state = foldRegion (rx, ry, rw, rh, env, state, topLeft)
            val state = foldRegion (rx, ry, rw, rh, env, state, topRight)
            val state = foldRegion (rx, ry, rw, rh, env, state, bottomLeft)
          in
            foldRegion (rx, ry, rw, rh, env, state, bottomRight)
          end
        else
          state
    | LEAF {items, x, y, w, h} =>
        if isCollidingPlus (rx, ry, rw, rh, x, y, w, h) then
          foldRegionVec (rx, ry, rw, rh, env, state, 0, items)
        else
          state
end
