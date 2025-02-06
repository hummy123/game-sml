structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree, enemies, graph} =
        game

      val enemyTree = Enemy.generateTree enemies
      val player = Player.runPhysicsAndInput (game, input, enemyTree)

    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      , graph = graph
      }
    end
end
