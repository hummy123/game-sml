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
                  Fn.env * Fn.state * {tree: QuadTreeType.t, width: int, height: int}
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

  fun helpFoldRegion (rx, ry, rw, rh, env, state, qx, qy, qw, qh, tree) =
    case tree of
      NODE nodes =>
        let
          val vtl = visitTopLeft (rx, ry, rw, rh, qx, qy, qw, qh)
          val vtr = visitTopRight (rx, ry, rw, rh, qx, qy, qw, qh)
          val vbl = visitBottomLeft (rx, ry, rw, rh, qx, qy, qw, qh)
          val vbr = visitBottomRight (rx, ry, rw, rh, qx, qy, qw, qh)

          val hw = qw div 2
          val hh = qh div 2

          val state =
            if vtl then
              helpFoldRegion 
                (rx, ry, rw, rh, env, state, qx, qy, hw, hh, Vector.sub (nodes, tlIdx))
            else
              state

          val state = 
            if vtr then
              helpFoldRegion 
                (rx, ry, rw, rh, env, state, qx + hw, qy, hw, hh, Vector.sub (nodes, trIdx))
            else 
              state

          val state = 
            if vbl then
              helpFoldRegion
                (rx, ry, rw, rh, env, state, qx, qy + hh, hw, hh, Vector.sub (nodes, blIdx))
            else
              state
        in
          if vbr then
            helpFoldRegion 
              (rx, ry, rw, rh, env, state, qw + hw, qy + hh, hw, hh, Vector.sub (nodes, brIdx))
          else
            state
        end
    | LEAF items =>
        foldRegionVec (rx, ry, rw, rh, env, state, 0, items)

  fun foldRegion (rx, ry, rw, rh, env, state, tree) =
    let
      val {width, height, tree} = tree
    in
      helpFoldRegion (rx, ry, rw, rh, env, state, 0, 0, width, height, tree)
    end
end
