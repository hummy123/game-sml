structure PlayerAttack =
struct
  (* - Handle collisions where player hits enemy directly - *)
  structure AttackEnemies =
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

         fun onPlayerAttack (enemyID, enemy, enemyMap, defeatedList) =
           case #variant enemy of
             PATROL_SLIME => defeatEnemy (enemyID, enemyMap, defeatedList)
           | FOLLOW_SLIME => defeatEnemy (enemyID, enemyMap, defeatedList)
           | STRAIGHT_BAT => defeatEnemy (enemyID, enemyMap, defeatedList)

         fun fold (enemyID, (), (defeatedList, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onPlayerAttack (enemyID, enemy, enemyMap, defeatedList)
           | NONE => (defeatedList, enemyMap)
       end)

  fun attackEnemies (player: PlayerType.player, enemyMap, enemyTree) =
    let
      open PlayerType
      val {x, y, facing, mainAttack, ...} = player
    in
      case mainAttack of
        MAIN_ATTACKING {length, ...} =>
          let
            open EntityType
            val height = Constants.playerSize
            val x =
              (case facing of
                 FACING_RIGHT => x + Constants.playerSize
               | FACING_LEFT => x - length)

            val (defeatedList, enemyMap) = AttackEnemies.foldRegion
              (x, y, length, height, (), ([], enemyMap), enemyTree)

            val defeatedList = Vector.fromList defeatedList
            val defeatedList = Vector.concat [defeatedList, #enemies player]

            val player =
              PlayerPatch.withPatch (player, PlayerPatch.W_ENEMIES defeatedList)
          in
            (player, enemyMap)
          end
      | _ => (player, enemyMap)
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

         fun onProjectileAttack (enemyID, enemy, enemyMap, fallingMap) =
           case #variant enemy of
             PATROL_SLIME => onDefeated (enemyID, enemy, enemyMap, fallingMap)
           | FOLLOW_SLIME => onDefeated (enemyID, enemy, enemyMap, fallingMap)
           | STRAIGHT_BAT => onDefeated (enemyID, enemy, enemyMap, fallingMap)

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
