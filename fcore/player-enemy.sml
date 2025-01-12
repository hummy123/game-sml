structure PlayerEnemy =
struct
  open GameType
  open PlayerPatch

  fun checkCollisions (player, enemies, enemyTree, projectiles) =
    let
      val {x, y, mainAttack, attacked, ...} = player
      val size = Constants.playerSize
      val projectileTree = Projectile.generateTree projectiles
    in
      case mainAttack of
        MAIN_ATTACKING =>
          let
            (* when attacking, player collision should be larger than player themselves *)
            val x = x - Constants.halfPlayerSize
            val y = y - Constants.halfPlayerSize
            val size = size * 2

            (* get list of enemies player has collided with *)
            val enemyCollisions = QuadTree.getCollisions
              (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)

            (* filter enemies based on collisions *)
            val enemyCollisions = Vector.fromList enemyCollisions
            val enemies = Enemy.filterWhenAttacked
              ( Vector.length enemies - 1
              , enemyCollisions
              , enemies
              , projectileTree
              , []
              )

            (* add collided enemies to player record, 
             * concatenating with the previous enemies defeated *)
            val player = Player.concatAttackedEnemies (player, enemyCollisions)
          in
            (player, enemies)
          end
      | _ =>
          (case attacked of
             NOT_ATTACKED =>
               let
                 val enemyCollisions = QuadTree.getCollisions
                   (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)

                 val player =
                   Player.enemyCollisionReaction
                     (player, enemies, enemyCollisions, [])

                 val enemies = Enemy.filterProjectileCollisions
                   (Vector.length enemies - 1, enemies, projectileTree, [])
               in
                 (player, enemies)
               end
           | ATTACKED amt =>
               if amt = Constants.attackedLimit then
                 (* if reached limit, detect enemies again *)
                 let
                   val enemyCollisions = QuadTree.getCollisions
                     (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)

                   val player =
                     Player.exitAttackedAndCheckEnemies
                       (player, enemies, enemyCollisions)

                   val enemies = Enemy.filterProjectileCollisions
                     (Vector.length enemies - 1, enemies, projectileTree, [])
                 in
                   (player, enemies)
                 end
               else
                 (* if attacked, don't detect collisions, 
                  * allowing a brief invincibility period as is common in many games 
                  * *)
                 let
                   val player = Player.incrementAttacked (player, amt)
                   val enemies = Enemy.filterProjectileCollisions
                     (Vector.length enemies - 1, enemies, projectileTree, [])
                 in
                   (player, enemies)
                 end)
    end
end
