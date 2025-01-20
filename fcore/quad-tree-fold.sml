signature QUAD_FOLDER =
sig
  type env
  type state
  val fState: state * env * int -> state
end

functor MakeQuadFolder(Fn: QUAD_FOLDER) =
struct
  open QuadTreeType

  fun visitTopLeft (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val midX = qW div 2 + qX
      val midY = qH div 2 + qY
    in
      iX <= midX andalso iY <= midY
    end

  fun visitTopRight (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val midX = qW div 2 + qX
      val midY = qH div 2 + qY
    in
      iX >= midX andalso iY <= midY
    end

  fun visitBottomLeft (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val midX = qW div 2 + qX
      val midY = qH div 2 + qY

      val iFinishY = iY + iH
    in
      iX <= midX andalso iFinishY >= midY
    end

  fun visitBottomRight (iX, iY, iW, iH, qX, qY, qW, qH) =
    let
      val midX = qW div 2 + qX
      val midY = qH div 2 + qY

      val iFinishX = iX + iH
      val iFinishY = iY + iH
    in
      iFinishX >= midX andalso iFinishY >= midY
    end

  fun foldVec (iX, iY, iW, iH, pos, elements, state, env) =
    if pos = Vector.length elements then
      state
    else
      let
        val {itemID, ...} = Vector.sub (elements, pos)
        val _ = print ("foldVec itemID: " ^ Int.toString itemID ^ "\n")
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

          val midX = halfW + quadX
          val midY = halfH + quadY

          val iX = itemX
          val iY = itemY
          val iW = itemW
          val iH = itemH

          val qX = quadX
          val qY = quadY
          val qW = quadW
          val qH = quadH

          val vtl = visitTopLeft (iX, iY, iW, iH, qX, qY, qW, qH)
          val vtr = visitTopRight (iX, iY, iW, iH, qX, qY, qW, qH)
          val vbl = visitBottomLeft (iX, iY, iW, iH, qX, qY, qW, qH)
          val vbr = visitBottomRight (iX, iY, iW, iH, qX, qY, qW, qH)

          val state =
            if vtl then
              foldRegion
                (iX, iY, iW, iH, qX, qY, halfW, halfH, env, state, topLeft)
            else
              state

          val state =
            if vtr then
              foldRegion
                ( itemX
                , itemY
                , itemW
                , itemH
                , midX
                , quadY
                , halfW
                , halfH
                , env
                , state
                , topRight
                )
            else
              state

          val state =
            if vbl then
              foldRegion
                ( itemX
                , itemY
                , itemW
                , itemH
                , quadX
                , midY
                , halfW
                , halfH
                , env
                , state
                , bottomLeft
                )
            else
              state
        in
          if vbr then
            foldRegion
              ( itemX
              , itemY
              , itemW
              , itemH
              , midX
              , midY
              , halfW
              , halfH
              , env
              , state
              , bottomRight
              )
          else
            state
        end
    | LEAF elements =>
        foldVec (itemX, itemY, itemW, itemH, 0, elements, state, env)
end
