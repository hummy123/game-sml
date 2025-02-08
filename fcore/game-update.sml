structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree, enemies, graph} =
        game

      val enemyTree = Enemy.generateTree enemies
      val player = Player.runPhysicsAndInput (game, input, enemyTree)

      val projectiles = #projectiles player
      val projectileTree = Projectile.generateTree projectiles

      val enemies = Enemy.updateEnemyList
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
        )
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
