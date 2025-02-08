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

      val (enemies, newFallingEnemies) = Enemy.updateEnemyList
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
        , []
        )

      val fallingEnemies = Vector.fromList newFallingEnemies
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
