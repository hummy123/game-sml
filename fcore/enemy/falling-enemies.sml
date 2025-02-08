structure FallingEnemies =
struct
  open GameType

  fun helpGenerateTree (pos, fallingVec: falling_enemy vector, acc) =
    if pos = Vector.length fallingVec then
      acc
    else
      let
        val {x, y, ...} = Vector.sub (fallingVec, pos)

        val size = Constants.enemySize

        val acc = QuadTree.insert (x, y, size, size, pos + 1, acc)
      in
        helpGenerateTree (pos + 1, fallingVec, acc)
      end

  fun generateTree fallingVec =
    helpGenerateTree
      ( 0
      , fallingVec
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  fun updateList (pos, vec, acc) =
    if pos < 0 then
      acc
    else
      let
        val {x, y, jumped, variant} = Vector.sub (vec, pos)

        val size = Constants.enemySize
        val ww = Constants.worldWidth
        val wh = Constants.worldHeight
      in
        if Collision.isColliding (x, y, size, size, 0, 0, ww, wh) then
          (* move falling enemy up or down depending on jumped *)
          let
            val updated =
              if jumped < Constants.jumpLimit then
                { x = x
                , y = y - Constants.moveEnemyBy
                , jumped = jumped + Constants.moveEnemyBy
                , variant = variant
                }
              else
                { x = x
                , y = y + Constants.moveEnemyBy
                , jumped = jumped
                , variant = variant
                }
          in
            updateList (pos - 1, vec, updated :: acc)
          end
        else
          (* if current is not colliding with world's bounds, then filter out 
           * as it is off screen *)
          updateList (pos - 1, vec, acc)
      end

  fun helpGetDrawVec
    (pos, fallingVec, width, height, ratio, xOffset, yOffset, acc) =
    if pos = Vector.length fallingVec then
      Vector.concat acc
    else
      let
        val {x, y, variant = _, jumped = _} = Vector.sub (fallingVec, pos)

        val x = Real32.fromInt x * ratio + xOffset
        val y = Real32.fromInt y * ratio + yOffset
        val size = Real32.fromInt Constants.enemySize * ratio

        val vec = Block.lerp (x, y, size, size, width, height, 0.3, 0.3, 0.3)
        val acc = vec :: acc
      in
        helpGetDrawVec
          (pos + 1, fallingVec, width, height, ratio, xOffset, yOffset, acc)
      end

  fun getDrawVec (game: game_type, width, height) =
    if Vector.length (#fallingEnemies game) = 0 then
      Vector.fromList []
    else
      let
        val fallingEnemies = #fallingEnemies game
        val wratio = width / Constants.worldWidthReal
        val hratio = height / Constants.worldHeightReal
      in
        if wratio < hratio then
          let
            val scale = Constants.worldHeightReal * wratio
            val yOffset =
              if height > scale then (height - scale) / 2.0
              else if height < scale then (scale - height) / 2.0
              else 0.0
          in
            helpGetDrawVec
              (0, fallingEnemies, width, height, wratio, 0.0, yOffset, [])
          end
        else
          let
            val scale = Constants.worldWidthReal * hratio
            val xOffset =
              if width > scale then (width - scale) / 2.0
              else if width < scale then (scale - width) / 2.0
              else 0.0
          in
            helpGetDrawVec
              (0, fallingEnemies, width, height, hratio, xOffset, 0.0, [])
          end
      end
end
