structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree, enemies, enemyTree} =
        game

      val player = Player.runPhysicsAndInput (game, input)

      (* check player-enemy collisions and react *)
      val (player, enemies) = PlayerEnemy.checkCollisions (player, game)

      (* create enemy quad tree from list of new enemies *)
      val enemyTree = Enemy.generateTree enemies
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
