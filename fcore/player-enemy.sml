structure PlayerEnemy =
struct
  open GameType

  fun getEnemyRecoilPatches (player, playerOnRight, acc) =
    if playerOnRight then
      let
        val newRecoil = RECOIL_RIGHT 0
        val newAttacked = ATTACKED 0
      in
        W_RECOIL newRecoil :: W_ATTACKED newAttacked :: W_FACING FACING_LEFT
        :: W_Y_AXIS FALLING :: W_X_AXIS STAY_STILL :: acc
      end
    else
      let
        val newRecoil = RECOIL_LEFT 0
        val newAttacked = ATTACKED 0
      in
        W_RECOIL newRecoil :: W_ATTACKED newAttacked :: W_FACING FACING_RIGHT
        :: W_Y_AXIS FALLING :: W_X_AXIS STAY_STILL :: acc
      end

  fun checkEnemies (player: player, enemies: enemy vector, lst, acc) =
    case lst of
      id :: tl =>
        let
          val playerOnRight =
            (* check if collision is closer to left side of enemy or right
             * and then chose appropriate direction to recoil in *)
            let
              val {x, ...} = player
              val pFinishX = x + Constants.playerSize
              val pHalfW = Constants.playerSize div 2
              val pCentreX = x + pHalfW

              val {x = ex, y = ey, ...} = Enemy.find (id, enemies)
              val eFinishX = ex + Enemy.size
              val eHalfW = Enemy.size div 2
              val eCentreX = ex + eHalfW
            in
              eCentreX < pCentreX
            end

          val acc = getEnemyRecoilPatches (player, playerOnRight, acc)
        in
          checkEnemies (player, enemies, tl, acc)
        end
    | [] => acc

  fun helpExists (pos, id, collisions) =
    if pos = Vector.length collisions then
      false
    else
      let val current = Vector.sub (collisions, pos)
      in current = id orelse helpExists (pos + 1, id, collisions)
      end

  fun exists (id, collisions) = helpExists (0, id, collisions)

  (* removes enemies from `enemies` vector when player is attacking that enemy
   * and also filter enemy (or change enemyh health)
   * if enemy has collided with projectile *)
  fun filterEnemyAttacked
    (pos, collisions, enemies: enemy vector, projectileTree, acc) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)
        val acc =
          if exists (#id enemy, collisions) then (* filter out *) acc
          else Enemy.onCollisionWithProjectile (enemy, projectileTree, acc)
      in
        filterEnemyAttacked (pos - 1, collisions, enemies, projectileTree, acc)
      end

  (* filter enemy projectiles when player is not attacking *)
  fun filterEnemyProjectiles (pos, enemies, projectileTree, acc) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)
        val acc = Enemy.onCollisionWithProjectile (enemy, projectileTree, acc)
      in
        filterEnemyProjectiles (pos - 1, enemies, projectileTree, acc)
      end

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
            val enemies = filterEnemyAttacked
              ( Vector.length enemies - 1
              , enemyCollisions
              , enemies
              , projectileTree
              , []
              )
            val enemyTree = Enemy.generateTree enemies

            (* add collided enemies to player record, 
             * concatenating with the previous enemies defeated *)
            val newDefeated =
              Vector.map (fn id => {angle = 360}) enemyCollisions
            val oldDefeated = #enemies player
            val allDefeated = Vector.concat [oldDefeated, newDefeated]
            val player = Player.withPatches (player, [W_ENEMIES allDefeated])
          in
            (player, enemies, enemyTree)
          end
      | _ =>
          (case attacked of
             NOT_ATTACKED =>
               let
                 val enemyCollisions = QuadTree.getCollisions
                   (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)

                 val patches =
                   checkEnemies (player, enemies, enemyCollisions, [])
                 val player = Player.withPatches (player, patches)

                 val enemies = filterEnemyProjectiles
                   (Vector.length enemies - 1, enemies, projectileTree, [])
               in
                 (player, enemies, enemyTree)
               end
           | ATTACKED amt =>
               if amt = Constants.attackedLimit then
                 (* if reached limit, detect enemies again *)
                 let
                   val enemyCollisions = QuadTree.getCollisions
                     (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)
                   val lst = [W_ATTACKED NOT_ATTACKED]
                   val patches =
                     checkEnemies (player, enemies, enemyCollisions, lst)
                   val player = Player.withPatches (player, patches)

                   val enemies = filterEnemyProjectiles
                     (Vector.length enemies - 1, enemies, projectileTree, [])
                 in
                   (player, enemies, enemyTree)
                 end
               else
                 (* if attacked, don't detect collisions, 
                  * allowing a brief invincibility period as is common in many games 
                  * *)
                 let
                   val amt = amt + 1
                   val attacked = ATTACKED amt
                   val player = Player.withPatches
                     (player, [W_ATTACKED attacked])

                   val enemies = filterEnemyProjectiles
                     (Vector.length enemies - 1, enemies, projectileTree, [])
                 in
                   (player, enemies, enemyTree)
                 end)
    end
end
