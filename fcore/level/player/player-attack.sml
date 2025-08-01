structure PlayerAttack =
struct
  (* - Handle collisions where player hits enemy directly - *)
  structure PlayerAttackEnemy =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = PlayerType.defeated_enemies list * EnemyMap.t

         open EnemyType

         fun defeatEnemy (enemyID, enemyMap, defeatedList) =
           let
             val defeatedList = {angle = 1} :: defeatedList
             val enemyMap = EnemyMap.remove (enemyID, enemyMap)
           in
             (defeatedList, enemyMap)
           end

         fun shieldSlimeAttacked (enemyID, enemy, enemyMap, defeatedList) =
           if #shieldOn enemy then (defeatedList, enemyMap)
           else defeatEnemy (enemyID, enemyMap, defeatedList)

         fun onPlayerAttack (enemyID, enemy, enemyMap, defeatedList) =
           case #variant enemy of
             PATROL_SLIME => defeatEnemy (enemyID, enemyMap, defeatedList)
           | FOLLOW_SLIME => defeatEnemy (enemyID, enemyMap, defeatedList)
           | STRAIGHT_BAT => defeatEnemy (enemyID, enemyMap, defeatedList)
           | SHIELD_SLIME =>
               shieldSlimeAttacked (enemyID, enemy, enemyMap, defeatedList)

         fun fold (enemyID, (), (defeatedList, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onPlayerAttack (enemyID, enemy, enemyMap, defeatedList)
           | NONE => (defeatedList, enemyMap)
       end)

  structure PlayerAttackFalling =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = PlayerType.defeated_enemies list * FallingEnemyMap.t

         fun fold (fallingID, (), (defeatedList, fallingMap)) =
           let
             val defeatedList = {angle = 1} :: defeatedList
             val fallingMap = FallingEnemyMap.remove (fallingID, fallingMap)
           in
             (defeatedList, fallingMap)
           end
       end)

  fun helpAttackEnemies
    (player, defeatedList, enemyMap, fallingMap, enemyTree, pos, boxes) =
    if pos = Vector.length boxes then
      let
        val defeatedList = Vector.fromList defeatedList
        val defeatedList = Vector.concat [defeatedList, #enemies player]

        val player =
          PlayerPatch.withPatch (player, PlayerPatch.W_ENEMIES defeatedList)
      in
        (player, enemyMap, fallingMap)
      end
    else
      let
        val {x = px, y = py, ...} = player
        val {x = bx, y = by} = Vector.sub (boxes, pos)

        val x = px + bx
        val y = py + by
        val size = Whip.size

        val (defeatedList, enemyMap) = PlayerAttackEnemy.foldRegion
          (x, y, size, size, (), (defeatedList, enemyMap), enemyTree)

        val fallingTree = FallingEnemies.generateTree fallingMap
        val (defeatedList, fallingMap) = PlayerAttackFalling.foldRegion
          (x, y, size, size, (), (defeatedList, fallingMap), fallingTree)
      in
        helpAttackEnemies
          ( player
          , defeatedList
          , enemyMap
          , fallingMap
          , enemyTree
          , pos + 1
          , boxes
          )
      end

  fun attackEnemies (player: PlayerType.player, enemyMap, enemyTree, fallingMap) =
    let
      open PlayerType
      val {x, y, facing, mainAttack, ...} = player
    in
      case mainAttack of
        MAIN_ATTACKING amt =>
          let
            open EntityType
            val frame = amt div 2

            val boxes =
              case facing of
                FACING_RIGHT => Vector.sub (Whip.rightFrames, frame)
              | FACING_LEFT => Vector.sub (Whip.leftFrames, frame)
          in
            helpAttackEnemies
              (player, [], enemyMap, fallingMap, enemyTree, 0, boxes)
          end
      | _ => (player, enemyMap, fallingMap)
    end

  (* - Handle collisions when player's projectile hits enemy - *)
  structure ProjectileHitEnemy =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = FallingEnemyMap.t * EnemyMap.t

         open EnemyType

         fun onDefeated (enemyID, enemy, enemyMap, fallingMap) =
           let
             val {x, y, variant, ...} = enemy
             val fallingItem = {x = x, y = y, variant = variant}
             val fallingMap =
               FallingEnemyMap.add (enemyID, fallingItem, fallingMap)
             val enemyMap = EnemyMap.remove (enemyID, enemyMap)
           in
             (fallingMap, enemyMap)
           end

         fun onShieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap) =
           if #shieldOn enemy then (fallingMap, enemyMap)
           else onDefeated (enemyID, enemy, enemyMap, fallingMap)

         fun onProjectileAttack (enemyID, enemy, enemyMap, fallingMap) =
           case #variant enemy of
             PATROL_SLIME => onDefeated (enemyID, enemy, enemyMap, fallingMap)
           | FOLLOW_SLIME => onDefeated (enemyID, enemy, enemyMap, fallingMap)
           | STRAIGHT_BAT => onDefeated (enemyID, enemy, enemyMap, fallingMap)
           | SHIELD_SLIME =>
               onShieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap)

         fun fold (enemyID, (), (fallingMap, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onProjectileAttack (enemyID, enemy, enemyMap, fallingMap)
           | NONE => (fallingMap, enemyMap)
       end)

  fun helpProjectileHitEnemy (pos, projectiles, enemyTree, enemyMap, fallingMap) =
    if pos = Vector.length projectiles then
      (fallingMap, enemyMap)
    else
      let
        val {x, y, ...}: PlayerType.player_projectile =
          Vector.sub (projectiles, pos)
        val size = Constants.projectileSizeInt
        val (fallingMap, enemyMap) = ProjectileHitEnemy.foldRegion
          (x, y, size, size, (), (fallingMap, enemyMap), enemyTree)
      in
        helpProjectileHitEnemy
          (pos + 1, projectiles, enemyTree, enemyMap, fallingMap)
      end

  fun projectileHitEnemy (projectiles, enemyMap, enemyTree, fallingMap) =
    helpProjectileHitEnemy (0, projectiles, enemyTree, enemyMap, fallingMap)
end
