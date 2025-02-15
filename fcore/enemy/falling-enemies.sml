structure FallingEnemies =
struct
  open EnemyType
  open EntityType

  (* - Generating tree of falling enemies - *)
  structure FallingTree =
    MakeGapMapFolder
      (struct
         structure Pair = FallingEnemyPair

         type env = unit
         type state = QuadTree.t

         fun fold (fallingID, falling: EnemyType.falling_enemy, (), quadTree) =
           let
             val {x, y, ...} = falling
             val size = Constants.enemySize
           in
             QuadTree.insert (x, y, size, size, fallingID, quadTree)
           end
       end)

  fun generateTree falling =
    FallingTree.foldUnordered
      ( falling
      , ()
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  (* - Updating position of fallingEnemies 
   * - and filtering out enemies which are no longer in world bounds - *)
  structure UpdateFalling =
    MakeGapMapFolder
      (struct
         structure Pair = FallingEnemyPair

         type env = unit

         type state = FallingEnemyMap.t

         fun fold (fallingID, fallingEnemy, (), fallingMap) =
           let
             val {x, y, variant} = fallingEnemy
             val size = Constants.enemySize
             val ww = Constants.worldWidth
             val wh = Constants.worldHeight
           in
             if Collision.isCollidingPlus (x, y, size, size, 0, 0, ww, wh) then
               let
                 val newFalling =
                   {x = x, y = y - Constants.moveEnemyBy, variant = variant}
               in
                 FallingEnemyMap.add (fallingID, newFalling, fallingMap)
               end
             else
               (* filter out since not in world bounds *)
               fallingMap
           end
       end)

  fun update fallingEnemies =
    UpdateFalling.foldUnordered (fallingEnemies, (), FallingEnemyMap.empty)

  (* - Drawing falling enemies - *)
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
