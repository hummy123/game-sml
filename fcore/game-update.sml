structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree, enemies, enemyTree} =
        game

      val player = Player.runPhysicsAndInput (game, input)

      (* check and react to enemy collisions with player projectile *)
      val enemies =
        ProjectileEnemy.checkCollisions
          (#projectiles player, enemies, enemyTree)
      val enemyTree = Enemy.generateTree enemies

      (* check player-enemy collisions and react *)
      val (player, enemies, enemyTree) =
        PlayerEnemy.checkCollisions (player, enemies, enemyTree)
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      , enemyTree = enemyTree
      }
    end
end
