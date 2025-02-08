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

      val projectiles = #projectiles player
      val projectileTree = Projectile.generateTree projectiles

      (* update state of falling enemies and possibly filter *)
      val fallingEnemies = FallingEnemies.updateList
        (Vector.length fallingEnemies - 1, fallingEnemies, [])

      val (enemies, fallingEnemies) = Enemy.updateEnemyList
        ( Vector.length enemies - 1
        , enemies
        , projectiles
        , projectileTree
        , walls
        , wallTree
        , platforms
        , platformTree
        , player
        , graph
        , []
        , fallingEnemies
        )

      val fallingEnemies = Vector.fromList fallingEnemies
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
