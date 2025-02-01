signature POINTER_QUAD_FOLDER =
sig
  type env
  type state

  val fold: int * env * state -> state
end

signature MAKE_POINTER_QUAD_TREE_FOLD =
sig
  structure Fn: POINTER_QUAD_FOLDER

  val foldRegion: int * int * int * int * 
                  Fn.env * Fn.state * {tree: PointerQuadTreeType.t, width: int, height: int}
                  -> Fn.state
end

functor MakePointerQuadTreeFold(Fn: POINTER_QUAD_FOLDER): MAKE_POINTER_QUAD_TREE_FOLD =
struct
  structure Fn = Fn

  open PointerQuadTreeType

  fun foldRegionList (rx, ry, rw, rh, env, state, lst) =
    case lst of
      item :: tl =>
        let
          val state =
            if isCollidingItem (rx, ry, rw, rh, ~1, item) then
              Fn.fold (#itemID item, env, state)
            else
              state
        in
          foldRegionList (rx, ry, rw, rh, env, state, tl)
        end
    | [] => state

  fun helpFoldRegion (rx, ry, rw, rh, env, state, qx, qy, qw, qh, tree) =
    case tree of
      NODE {tl, tr, bl, br} =>
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
                (rx, ry, rw, rh, env, state, qx, qy, hw, hh, tl)
            else
              state

          val state = 
            if vtr then
              helpFoldRegion 
                (rx, ry, rw, rh, env, state, qx + hw, qy, hw, hh, tr)
            else 
              state

          val state = 
            if vbl then
              helpFoldRegion
                (rx, ry, rw, rh, env, state, qx, qy + hh, hw, hh, bl)
            else
              state
        in
          if vbr then
            helpFoldRegion 
              (rx, ry, rw, rh, env, state, qw + hw, qy + hh, hw, hh, br)
          else
            state
        end
    | LEAF item =>
        if isCollidingItem (rx, ry, rw, rh, ~1, item) then
          Fn.fold (#itemID item, env, state)
        else
          state
    | EMPTY => state
    | SHARE_LEAF items => foldRegionList (rx, ry, rw, rh, env, state, items)

  fun foldRegion (rx, ry, rw, rh, env, state, tree) =
    let
      val {width, height, tree} = tree
    in
      helpFoldRegion (rx, ry, rw, rh, env, state, 0, 0, width, height, tree)
    end
end

