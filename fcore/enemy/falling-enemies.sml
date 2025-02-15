structure FallingEnemies =
struct
  open EnemyType
  open EntityType

  structure FallingDrawVec =
    MakeGapMapFolder
      (struct
         structure Pair = FallingEnemyPair

         type env =
           { width: Real32.real
           , height: Real32.real
           , ratio: Real32.real
           , xOffset: Real32.real
           , yOffset: Real32.real
           }

         type state = Real32.real vector list

         fun helpGetDrawVec
           (fallingEnemy, width, height, ratio, xOffset, yOffset, acc) =
           let
             val {x, y, variant = _} = fallingEnemy

             val x = Real32.fromInt x * ratio + xOffset
             val y = Real32.fromInt y * ratio + yOffset
             val size = Real32.fromInt Constants.enemySize * ratio

             val vec = Block.lerp
               (x, y, size, size, width, height, 0.3, 0.3, 0.3)
           in
             vec :: acc
           end

         fun fold (_, fallingEnemy, env, acc) =
           let
             val {width, height, ratio, xOffset, yOffset} = env
           in
             helpGetDrawVec
               (fallingEnemy, width, height, ratio, xOffset, yOffset, acc)
           end
       end)

  fun getDrawVec (game: GameType.game_type, width, height) =
    let
      val fallingEnemies = #fallingEnemies game
      val wratio = width / Constants.worldWidthReal
      val hratio = height / Constants.worldHeightReal

      val env =
        if wratio < hratio then
          let
            val scale = Constants.worldHeightReal * wratio
            val yOffset =
              if height > scale then (height - scale) / 2.0
              else if height < scale then (scale - height) / 2.0
              else 0.0
          in
            { width = width
            , height = height
            , ratio = wratio
            , xOffset = 0.0
            , yOffset = yOffset
            }
          end
        else
          let
            val scale = Constants.worldWidthReal * hratio
            val xOffset =
              if width > scale then (width - scale) / 2.0
              else if width < scale then (scale - width) / 2.0
              else 0.0
          in
            { width = width
            , height = height
            , ratio = hratio
            , xOffset = xOffset
            , yOffset = 0.0
            }
          end

      val lst = FallingDrawVec.foldUnordered (fallingEnemies, env, [])
    in
      Vector.concat lst
    end
end
