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
end
