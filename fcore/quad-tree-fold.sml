signature QUAD_FOLDER =
sig
  type env
  type state
  val fState: state * env * int -> state
end

functor MakeQuadFolder(Fn: QUAD_FOLDER) =
struct
  open QuadTreeType

  fun foldVec (iX, iY, iW, iH, pos, elements, state, env) =
    if pos = Vector.length elements then
      state
    else
      let
        val {itemID, ...} = Vector.sub (elements, pos)
        val state = Fn.fState (state, env, itemID)
      in
        foldVec (iX, iY, iW, iH, pos + 1, elements, state, env)
      end

  fun foldRegion
    ( itemX
    , itemY
    , itemW
    , itemH
    , quadX
    , quadY
    , quadW
    , quadH
    , env: Fn.env
    , state: Fn.state
    , tree: QuadTreeType.t
    ) =
    case tree of
      NODE {topLeft, topRight, bottomLeft, bottomRight, elements} =>
        let
          (* fold over intersecting elements in this vector first *)
          val state = foldVec
            (itemX, itemY, itemW, itemH, 0, elements, state, env)

          val halfW = quadW div 2
          val halfH = quadH div 2
        in
          (case
             QuadTree.whichQuadrant
               (itemX, itemY, itemW, itemH, quadX, quadY, quadW, quadH)
           of
             TOP_LEFT =>
               foldRegion
                 ( itemX
                 , itemY
                 , itemW
                 , itemH
                 , quadX
                 , quadY
                 , halfW
                 , halfH
                 , env
                 , state
                 , topLeft
                 )
           | TOP_RIGHT =>
               foldRegion
                 ( itemX
                 , itemY
                 , itemW
                 , itemH
                 , quadX + halfW (* middleX *)
                 , quadY
                 , halfW
                 , halfH
                 , env
                 , state
                 , topRight
                 )
           | BOTTOM_LEFT =>
               foldRegion
                 ( itemX
                 , itemY
                 , itemW
                 , itemH
                 , quadX
                 , quadY + halfH (* middleY *)
                 , halfW
                 , halfH
                 , env
                 , state
                 , bottomLeft
                 )
           | BOTTOM_RIGHT =>
               foldRegion
                 ( itemX
                 , itemY
                 , itemW
                 , itemH
                 , quadX + halfW (* middleX *)
                 , quadY + halfH (* middleY *)
                 , halfW
                 , halfH
                 , env
                 , state
                 , bottomRight
                 )
           | PARENT_QUADRANT => state)
        end
    | LEAF elements =>
        foldVec (itemX, itemY, itemW, itemH, 0, elements, state, env)
end
