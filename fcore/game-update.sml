structure GameUpdate =
struct
  fun update (game, input) =
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
        } = game

      val enemyTree = Enemy.generateTree enemies
      val player = Player.runPhysicsAndInput (game, input, enemyTree)

      val enemies = Enemy.update
        (enemies, walls, wallTree, platforms, platformTree, player, graph)

      (* update state of falling enemies and possibly filter *)
      (* todo: use enemy map
      val fallingEnemies = FallingEnemies.updateList
        (Vector.length fallingEnemies - 1, fallingEnemies, player, [])
      
      val fallingEnemies = Vector.fromList fallingEnemies
        *)
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      , graph = graph
      , fallingEnemies = fallingEnemies
      }
    end
end
