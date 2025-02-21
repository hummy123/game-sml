structure LevelUpdate =
struct
  fun update (level, input, userKeys, time) =
    let
      val
        { player
        , walls
        , wallTree
        , platforms
        , platformTree
        , enemies
        , graph
        , fallingEnemies
        } = level

      val player = Player.runPhysicsAndInput (level, input)

      val enemyTree = Enemy.generateTree enemies
      val player = Player.checkEnemyCollisions (player, enemies, enemyTree)

      val (player, enemies, fallingEnemies) =
        PlayerAttack.attackEnemies (player, enemies, enemyTree, fallingEnemies)

      val projectiles = #projectiles player
      val (fallingEnemies, enemies) =
        PlayerAttack.projectileHitEnemy
          (projectiles, enemies, enemyTree, fallingEnemies)

      val enemies = Enemy.update
        (enemies, walls, wallTree, platforms, platformTree, player, graph)

      val fallingEnemies = FallingEnemies.update fallingEnemies

      val mode =
        { player = player
        , walls = walls
        , wallTree = wallTree
        , platforms = platforms
        , platformTree = platformTree
        , enemies = enemies
        , graph = graph
        , fallingEnemies = fallingEnemies
        }
    in
      {mode = GameType.LEVEL mode, userKeys = userKeys}
    end
end
