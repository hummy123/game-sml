structure PlayerAttack =
struct
  (* - Handle collisions where player hits enemy directly - *)
  structure PlayerAttackEnemy =
    MakeQuadTreeFold
      (struct
         type env = Time.time
         type state = FallingEnemyMap.t * EnemyMap.t

         open EnemyType

         fun defeatEnemy (enemyID, enemyMap, fallingMap, time) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME (enemy: EnemyType.enemy) =>
               let
                 val {x, y, variant, ...} = enemy
                 val fallenEnemy =
                   {x = x, y = y, variant = variant, time = time}
                 val fallingMap =
                   FallingEnemyMap.add (enemyID, fallenEnemy, fallingMap)
                 val enemyMap = EnemyMap.remove (enemyID, enemyMap)
               in
                 (fallingMap, enemyMap)
               end
           | NONE => (fallingMap, enemyMap)

         fun shieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap, time) =
           if #shieldOn enemy then (fallingMap, enemyMap)
           else defeatEnemy (enemyID, enemyMap, fallingMap, time)

         fun onPlayerAttack (enemyID, enemy, enemyMap, fallingMap, time) =
           case #variant enemy of
             PATROL_SLIME => defeatEnemy (enemyID, enemyMap, fallingMap, time)
           | FOLLOW_SLIME => defeatEnemy (enemyID, enemyMap, fallingMap, time)
           | STRAIGHT_BAT => defeatEnemy (enemyID, enemyMap, fallingMap, time)
           | SHIELD_SLIME =>
               shieldSlimeAttacked (enemyID, enemy, enemyMap, fallingMap, time)

         fun fold (enemyID, time, (fallingMap, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onPlayerAttack (enemyID, enemy, enemyMap, fallingMap, time)
           | NONE => (fallingMap, enemyMap)
       end)

  structure PlayerAttackFalling =
    MakeQuadTreeFold
      (struct
         type env = Time.time
         type state = PlayerType.defeated_enemies list * FallingEnemyMap.t

         open Time

         fun fold (fallingID, mainAttackTime, (defeatedList, fallingMap)) =
           case FallingEnemyMap.get (fallingID, fallingMap) of
             SOME {time = fallingStartTime, ...} =>
               if mainAttackTime > fallingStartTime then
                 let
                   val defeatedList = {angle = 1} :: defeatedList
                   val fallingMap =
                     FallingEnemyMap.remove (fallingID, fallingMap)
                 in
                   (defeatedList, fallingMap)
                 end
               else
                 (defeatedList, fallingMap)
           | NONE => (defeatedList, fallingMap)
       end)

  fun helpAttackEnemies
    ( projectileX
    , projectileY
    , enemyMap
    , enemyTree
    , fallingMap
    , player
    , mainAttackTime
    ) =
    let
      val width = Constants.projectileWidth
      val height = Constants.projectileHeight

      val fallingTree = FallingEnemies.generateTree fallingMap
      val (defeatedList, fallingMap) = PlayerAttackFalling.foldRegion
        ( projectileX
        , projectileY
        , width
        , height
        , mainAttackTime
        , ([], fallingMap)
        , fallingTree
        )

      val (fallingMap, enemyMap) = PlayerAttackEnemy.foldRegion
        ( projectileX
        , projectileY
        , width
        , height
        , mainAttackTime
        , (fallingMap, enemyMap)
        , enemyTree
        )

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
        MAIN_ATTACKING {timePressed, ...} =>
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
                   , timePressed
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
                   , timePressed
                   )
               end)
      | _ => (player, enemyMap, fallingMap)
    end

  (* - Handle collisions when player's projectile hits enemy - *)
  structure ProjectileHitEnemy =
    MakeQuadTreeFold
      (struct
         type env = Time.time
         type state = FallingEnemyMap.t * EnemyMap.t

         open EnemyType

         fun onDefeated (enemyID, enemy, enemyMap, fallingMap, currentTime) =
           let
             val {x, y, variant, ...} = enemy
             val fallingItem =
               {x = x, y = y, variant = variant, time = currentTime}
             val fallingMap =
               FallingEnemyMap.add (enemyID, fallingItem, fallingMap)
             val enemyMap = EnemyMap.remove (enemyID, enemyMap)
           in
             (fallingMap, enemyMap)
           end

         fun onShieldSlimeAttacked
           (enemyID, enemy, enemyMap, fallingMap, currentTime) =
           if #shieldOn enemy then (fallingMap, enemyMap)
           else onDefeated (enemyID, enemy, enemyMap, fallingMap, currentTime)

         fun onProjectileAttack
           (enemyID, enemy, enemyMap, fallingMap, currentTime) =
           case #variant enemy of
             PATROL_SLIME =>
               onDefeated (enemyID, enemy, enemyMap, fallingMap, currentTime)
           | FOLLOW_SLIME =>
               onDefeated (enemyID, enemy, enemyMap, fallingMap, currentTime)
           | STRAIGHT_BAT =>
               onDefeated (enemyID, enemy, enemyMap, fallingMap, currentTime)
           | SHIELD_SLIME =>
               onShieldSlimeAttacked
                 (enemyID, enemy, enemyMap, fallingMap, currentTime)

         fun fold (enemyID, currentTime, (fallingMap, enemyMap)) =
           case EnemyMap.get (enemyID, enemyMap) of
             SOME enemy =>
               onProjectileAttack
                 (enemyID, enemy, enemyMap, fallingMap, currentTime)
           | NONE => (fallingMap, enemyMap)
       end)

  fun helpProjectileHitEnemy
    (pos, projectiles, enemyTree, enemyMap, fallingMap, currentTime) =
    if pos = Vector.length projectiles then
      (fallingMap, enemyMap)
    else
      let
        val {x, y, ...}: PlayerType.player_projectile =
          Vector.sub (projectiles, pos)
        val size = Constants.projectileSizeInt
        val (fallingMap, enemyMap) = ProjectileHitEnemy.foldRegion
          (x, y, size, size, currentTime, (fallingMap, enemyMap), enemyTree)
      in
        helpProjectileHitEnemy
          (pos + 1, projectiles, enemyTree, enemyMap, fallingMap, currentTime)
      end

  fun projectileHitEnemy
    (projectiles, enemyMap, enemyTree, fallingMap, currentTime) =
    helpProjectileHitEnemy
      (0, projectiles, enemyTree, enemyMap, fallingMap, currentTime)
end
