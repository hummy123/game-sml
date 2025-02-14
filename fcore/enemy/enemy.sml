structure Enemy =
struct
  (* - Updating state of enemies per loop - *)
  (*
  structure UpdateEnemies = MakeGapMapMapper (struct
    structure Pair = EnemyPair
    
    type env = {walls: wall vector, wallTree: QuadTree.t, platforms: platform
    vector, platformTree: QuadTree.t}
  
    (enemy, walls, wallTree, platforms, platformTree, player, graph) =
  end)
  *)

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
             val {x, y, ...} = enemy
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

                 val x = Real32.fromInt x * wratio
                 val y = Real32.fromInt y * wratio + yOffset

                 val realSize = Constants.enemySizeReal * wratio
               in
                 Block.lerp
                   (x, y, realSize, realSize, width, height, 0.5, 0.5, 1.0)
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
                 Block.lerp
                   (x, y, realSize, realSize, width, height, 0.5, 0.5, 1.0)
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
