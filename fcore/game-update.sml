structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree, enemies, enemyTree} =
        game

      val player = Player.runPhysicsAndInput (game, input)

      (* check player-enemy collisions and react *)
      val (player, enemies, enemyTree) =
        PlayerEnemy.checkCollisions
          (player, enemies, enemyTree, #projectiles player)

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
