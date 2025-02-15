structure Enemy =
struct
  (* - Updating state of enemies per loop - *)
  structure UpdateEnemies =
    MakeGapMapMapper
      (struct
         structure Pair = EnemyPair

         type env =
           { walls: Wall.t vector
           , wallTree: QuadTree.t
           , platforms: Platform.t vector
           , platformTree: QuadTree.t
           , player: PlayerType.player
           , graph: PlatSet.elem vector vector
           }

         type state = EnemyMap.t

         fun map (enemy, env) =
           let
             val {walls, wallTree, platforms, platformTree, player, graph} = env
           in
             EnemyBehaviour.updateEnemyState
               (enemy, walls, wallTree, platforms, platformTree, player, graph)
           end
       end)

  fun update (enemies, walls, wallTree, platforms, platformTree, player, graph) =
    let
      val env =
        { walls = walls
        , wallTree = wallTree
        , platforms = platforms
        , platformTree = platformTree
        , player = player
        , graph = graph
        }
    in
      UpdateEnemies.map (enemies, env)
    end

  (* - Generating enemy tree - *)
  structure EnemyTree =
    MakeGapMapFolder
      (struct
         structure Pair = EnemyPair

         type env = unit
         type state = QuadTree.t

         fun fold (enemyID, enemy: EnemyType.enemy, (), quadTree) =
           let
             val {id, x, y, ...} = enemy
             val size = Constants.enemySize
           in
             QuadTree.insert (x, y, size, size, id, quadTree)
           end
       end)

  fun generateTree enemies =
    EnemyTree.foldUnordered
      ( enemies
      , ()
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  (* - Drawing enemies - *)
  structure EnemyDrawVec =
    MakeGapMapFolder
      (struct
         structure Pair = EnemyPair

         type env = Real32.real * Real32.real
         type state = Real32.real vector list

         fun helpGetDrawVec (enemy: EnemyType.enemy, width, height) =
           let
             val {x, y, variant, ...} = enemy
             val wratio = width / Constants.worldWidthReal
             val hratio = height / Constants.worldHeightReal

             open EnemyType
             val (r, g, b) =
               case variant of
                 PATROL_SLIME => (0.5, 0.5, 1.0)
               | FOLLOW_SLIME => (1.0, 0.5, 0.5)
               | STRAIGHT_BAT => (0.55, 0.55, 0.55)
           in
             if wratio < hratio then
               let
                 val scale = Constants.worldHeightReal * wratio
                 val yOffset =
                   if height > scale then (height - scale) / 2.0
                   else if height < scale then (scale - height) / 2.0
                   else 0.0

                 val x = Real32.fromInt x * wratio
                 val y = Real32.fromInt y * wratio + yOffset

                 val realSize = Constants.enemySizeReal * wratio
               in
                 Block.lerp (x, y, realSize, realSize, width, height, r, g, b)
               end
             else
               let
                 val scale = Constants.worldWidthReal * hratio
                 val xOffset =
                   if width > scale then (width - scale) / 2.0
                   else if width < scale then (scale - width) / 2.0
                   else 0.0

                 val x = Real32.fromInt x * hratio + xOffset
                 val y = Real32.fromInt y * hratio

                 val realSize = Constants.enemySizeReal * hratio
               in
                 Block.lerp (x, y, realSize, realSize, width, height, r, g, b)
               end
           end

         fun fold (_, enemy: EnemyType.enemy, (width, height), acc) =
           helpGetDrawVec (enemy, width, height) :: acc
       end)

  fun getDrawVec (enemies, width, height) =
    let val vec = EnemyDrawVec.foldUnordered (enemies, (width, height), [])
    in Vector.concat vec
    end
end
