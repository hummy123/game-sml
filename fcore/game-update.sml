structure GameUpdate =
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
              val pFinishX = x + Player.size
              val pHalfW = Player.size div 2
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

  fun checkEnemiesWhileAttacking (player, enemies, lst, acc) =
    let
      open QuadTree
    in
      case lst of
        enemyID :: tl => (* placeholder *) acc
      | [] => acc
    end

  (* removes enemies from `enemies` vector when that enemy is in collisions *)
  fun filterEnemyCollisions (pos, collisions, enemies: enemy vector, acc) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)
      in
        if BinSearch.exists (#id enemy, collisions) then
          (* filter out *)
          filterEnemyCollisions (pos - 1, collisions, enemies, acc)
        else
          (* don't filter out *)
          filterEnemyCollisions (pos - 1, collisions, enemies, enemy :: acc)
      end

  fun checkPlayerEnemyCollisions (player, game) =
    let
      val {x, y, mainAttack, attacked, ...} = player
      val {enemies, enemyTree, ...} = game
      val size = Player.size
    in
      case mainAttack of
        MAIN_ATTACKING =>
          let
            (* when attacking, player collision should be larger than player themselves *)
            val x = x - Player.halfSize
            val y = y - Player.halfSize
            val size = size * 2

            val enemyCollisions = QuadTree.getCollisions
              (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)
            val patches =
              checkEnemiesWhileAttacking (player, enemies, enemyCollisions, [])
            val player = Player.withPatches (player, patches)

            val enemyCollisions = Vector.fromList enemyCollisions
            val enemies = filterEnemyCollisions
              (Vector.length enemies - 1, enemyCollisions, enemies, [])

            (* add collided enemies to player record, 
             * concatenating with the previous enemies defeated *)
            val newDefeated =
              Vector.map (fn id => {angle = 360}) enemyCollisions

            val oldDefeated = #enemies player
            val allDefeated = Vector.concat [oldDefeated, newDefeated]

            val player = Player.withPatches (player, [W_ENEMIES allDefeated])
          in
            (player, enemies)
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
               in
                 (player, enemies)
               end
           | ATTACKED amt =>
               if amt = Player.attackedLimit then
                 (* if reached limit, detect enemies again *)
                 let
                   val enemyCollisions = QuadTree.getCollisions
                     (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)
                   val lst = [W_ATTACKED NOT_ATTACKED]
                   val patches =
                     checkEnemies (player, enemies, enemyCollisions, lst)
                   val player = Player.withPatches (player, patches)
                 in
                   (player, enemies)
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
                 in
                   (player, enemies)
                 end)
    end

  fun update (game, input) =
    let
      val
        { player
        , playerProjectiles
        , walls
        , wallTree
        , platforms
        , platformTree
        , enemies
        , enemyTree
        } = game

      val player = Player.runPhysicsAndInput (game, input)

      (* check player-enemy collisions and react *)
      val (player, enemies) = checkPlayerEnemyCollisions (player, game)

      (* create enemy quad tree from list of new enemies *)
      val enemyTree = Enemy.generateTree enemies
    in
      { player = player
      , playerProjectiles = playerProjectiles
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      , enemies = enemies
      , enemyTree = enemyTree
      }
    end
end
