structure PlayerAttack =
struct
  (* - Handle collisions where player hits enemy directly - *)
  structure PlayerAttackEnemy =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = FallingEnemyMap.t * EnemyMap.t

         open EnemyType

         fun defeatEnemy (enemyID, enemyMap, fallingMap) =
           case EnemyMap.get (enemyID, enemyMap) of
              SOME (enemy: EnemyType.enemy) =>
                let
                  val {x, y, variant, ...} = enemy
                  val fallenEnemy = {x = x, y = y, variant = variant}
                  val fallingMap = FallingEnemyMap.add (enemyID, fallenEnemy, fallingMap)
                  val enemyMap = EnemyMap.remove (enemyID, enemyMap)
                in
                  (fallingMap, enemyMap)
                end
            | NONE => (fallingMap, enemyMap)

         fun shieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap) =
           if #shieldOn enemy then (fallingMap, enemyMap)
           else defeatEnemy (enemyID, enemyMap, fallingMap)

         fun onPlayerAttack (enemyID, enemy, enemyMap, fallingMap) =
           case #variant enemy of
             PATROL_SLIME => defeatEnemy (enemyID, enemyMap, fallingMap)
           | FOLLOW_SLIME => defeatEnemy (enemyID, enemyMap, fallingMap)
           | STRAIGHT_BAT => defeatEnemy (enemyID, enemyMap, fallingMap)
           | SHIELD_SLIME =>
               shieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap)

         fun fold (enemyID, (), (fallingMap, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onPlayerAttack (enemyID, enemy, enemyMap, fallingMap)
           | NONE => (fallingMap, enemyMap)
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
    (projectileX, projectileY, enemyMap, enemyTree, fallingMap, player) =
    let
      val width = Constants.projectileWidth
      val height = Constants.projectileHeight

      val fallingTree = FallingEnemies.generateTree fallingMap
      val (defeatedList, fallingMap) = PlayerAttackFalling.foldRegion
        ( projectileX
        , projectileY
        , width
        , height
        , ()
        , ([], fallingMap)
        , fallingTree
        )

      val (fallingMap, enemyMap) = PlayerAttackEnemy.foldRegion
        (projectileX, projectileY, width, height, (), (fallingMap, enemyMap), enemyTree)

      val defeatedList = Vector.fromList defeatedList
      val defeatedList = Vector.concat [defeatedList, #enemies player]
      val player =
        PlayerPatch.withPatch (player, PlayerPatch.W_ENEMIES defeatedList)
    in
      (player, enemyMap, fallingMap)
    end

  fun attackEnemies (player: PlayerType.player, enemyMap, enemyTree, fallingMap) =
    let
      open PlayerType
      open EntityType
      val {x, y, facing, mainAttack, ...} = player
    in
      case mainAttack of
        MAIN_ATTACKING _ =>
          (case facing of
             FACING_RIGHT =>
               let
                 val projectileX = x + Constants.playerWidth
                 val projectileY = y + Constants.projectileOffsetY
               in
                 helpAttackEnemies
                   ( projectileX
                   , projectileY
                   , enemyMap
                   , enemyTree
                   , fallingMap
                   , player
                   )
               end
           | FACING_LEFT =>
               let
                 val projectileX = x - Constants.projectileWidth
                 val projectileY = y + Constants.projectileOffsetY
               in
                 helpAttackEnemies
                   ( projectileX
                   , projectileY
                   , enemyMap
                   , enemyTree
                   , fallingMap
                   , player
                   )
               end)
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
