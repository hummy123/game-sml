structure Player =
struct
  open GameType

  datatype patch =
    W_X_AXIS of player_x_axis
  | W_Y_AXIS of player_y_axis
  | W_RECOIL of player_recoil
  | W_ATTACKED of player_attacked
  | W_MAIN_ATTACK of main_attack
  | W_FACING of facing
  | W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_JUMP_PRESSED of bool
  | W_MAIN_ATTACK_PRESSED of bool

  fun mkPlayer
    ( health
    , xAxis
    , yAxis
    , x
    , y
    , jumpPressed
    , recoil
    , attacked
    , mainAttack
    , facing
    , mainAttackPressed
    ) =
    { yAxis = yAxis
    , xAxis = xAxis
    , recoil = recoil
    , attacked = attacked
    , mainAttack = mainAttack
    , mainAttackPressed = mainAttackPressed
    , facing = facing
    , health = health
    , x = x
    , y = y
    , jumpPressed = jumpPressed
    }

  fun withPatch (player: player, patch) =
    let
      val
        { yAxis
        , xAxis
        , recoil
        , attacked
        , mainAttack
        , mainAttackPressed
        , facing
        , health
        , x
        , y
        , jumpPressed
        } = player
    in
      case patch of
        W_X_AXIS xAxis =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_Y_AXIS yAxis =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_RECOIL recoil =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_ATTACKED attacked =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_MAIN_ATTACK mainAttack =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_FACING facing =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_HEALTH health =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_X x =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_Y y =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_JUMP_PRESSED jumpPressed =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
      | W_MAIN_ATTACK_PRESSED mainAttackPressed =>
          mkPlayer
            ( health
            , xAxis
            , yAxis
            , x
            , y
            , jumpPressed
            , recoil
            , attacked
            , mainAttack
            , facing
            , mainAttackPressed
            )
    end

  fun withPatches (player: player, lst) =
    case lst of
      hd :: tl =>
        let val player = withPatch (player, hd)
        in withPatches (player, tl)
        end
    | [] => player

  (* width/height *)
  val size = 35
  val realSize = 35.0

  val moveBy = 5

  (* timing variables; always start at 0, 
   * and revert to default state when limit is hit *)
  val jumpLimit = 150
  val floatLimit = 3
  val recoilLimit = 15
  val attackedLimit = 55
  val mainAttackLimit = 15

  (* helper functions checking input *)
  fun getXAxis (lh, rh) =
    case (lh, rh) of
      (false, false) => STAY_STILL
    | (false, true) => MOVE_RIGHT
    | (true, false) => MOVE_LEFT
    | (true, true) => STAY_STILL

  fun getFacing (facing, xAxis) =
    case xAxis of
      STAY_STILL => facing
    | MOVE_LEFT => FACING_LEFT
    | MOVE_RIGHT => FACING_RIGHT

  (* function returns default yAxis when neither up/down are pressed
   * or both are pressed. 
   *
   * In the case where the user was previously jumping,
   * we enter the floating stage, because it's normal for games
   * to have a very brief floating/gliding period before applying gravity.
   *
   * In the case where the user was previously floating, we want the player to
   * keep floating at this point (another function will apply gravity if we
   * floated enough).
   *
   * In every other case, we return the FALLING variant,
   * which has the same effect as returning the ON_GROUND variant,
   * except that it means gravity is applied if we walk off a platform.
   * *)
  fun defaultYAxis prevAxis =
    case prevAxis of
      JUMPING _ => FLOATING 0
    | FLOATING _ => prevAxis
    | DROP_BELOW_PLATFORM => prevAxis
    | _ => FALLING

  (* We want to prevent a double jump
   * or jumping while the player is falling
   * so we only switch to the JUMPING case if the player
   * is on the ground. *)
  fun onJumpPressed (prevAxis, jumpPressed) =
    case prevAxis of
      ON_GROUND =>
        if jumpPressed then (* apply gravity *) FALLING else JUMPING 0
    | _ => prevAxis

  fun checkWalls (player, walls, lst, acc) =
    let
      open QuadTree
    in
      case lst of
        (QUERY_ON_LEFT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX + wallWidth
            val acc = W_X newX :: acc
          in
            checkWalls (player, walls, tl, acc)
          end
      | (QUERY_ON_RIGHT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX - size
            val acc = W_X newX :: acc
          in
            checkWalls (player, walls, tl, acc)
          end
      | (QUERY_ON_BOTTOM_SIDE, wallID) :: tl =>
          let
            val {y = wallY, ...} = Vector.sub (walls, wallID - 1)

            val newY = wallY - size
            val acc = W_Y_AXIS ON_GROUND :: W_Y newY :: acc
          in
            checkWalls (player, walls, tl, acc)
          end
      | (QUERY_ON_TOP_SIDE, wallID) :: tl => checkWalls (player, walls, tl, acc)
      | [] => acc
    end

  fun checkPlatforms (player, platforms, lst, acc) =
    let
      open QuadTree
    in
      case lst of
        platID :: tl =>
          (case #yAxis player of
             DROP_BELOW_PLATFORM =>
               (* pass through, allowing player to drop below the platform *)
               checkPlatforms (player, platforms, tl, acc)
           | JUMPING _ =>
               (* pass through, allowing player to jump above the platform *)
               checkPlatforms (player, platforms, tl, acc)
           | _ =>
               let
                 (* default case: 
                  * player will land on platform and stay on the ground there. *)
                 val {y = platY, ...} = Vector.sub (platforms, platID - 1)

                 val newY = platY - size
                 val acc = W_Y_AXIS ON_GROUND :: W_Y newY :: acc
               in
                 checkPlatforms (player, platforms, tl, acc)
               end)
      | [] => acc
    end

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
              val pFinishX = x + size
              val pHalfW = size div 2
              val pCentreX = x + pHalfW

              val {x = ex, y = ey, ...} = Vector.sub (enemies, id - 1)
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

  (* only checks for collisions with environment (walls and platforms) *)
  fun getEnvironmentPatches (player, game) =
    let
      val {walls, wallTree, platformTree, platforms, enemyTree, enemies, ...} =
        game

      val {x, y, attacked, mainAttack, ...} = player

      val platCollisions = QuadTree.getCollisionsBelow
        (x, y, size, size, 0, 0, 1920, 1080, 0, platformTree)
      val acc = checkPlatforms (player, platforms, platCollisions, [])

      val wallCollisions = QuadTree.getCollisionSides
        (x, y, size, size, 0, 0, 1920, 1080, 0, wallTree)
    in
      checkWalls (player, walls, wallCollisions, acc)
    end

  fun getMovePatches player =
    let
      val {xAxis, yAxis, x, y, ...} = player

      val desiredX =
        case xAxis of
          STAY_STILL => x
        | MOVE_LEFT => x - moveBy
        | MOVE_RIGHT => x + moveBy
    in
      case yAxis of
        ON_GROUND => [W_X desiredX]
      | FLOATING floated =>
          let
            val yAxis =
              if floated = floatLimit then FALLING else FLOATING (floated + 1)
          in
            [W_X desiredX, W_Y_AXIS yAxis]
          end
      | FALLING =>
          let val desiredY = y + moveBy
          in [W_X desiredX, W_Y desiredY]
          end
      | DROP_BELOW_PLATFORM =>
          let val desiredY = y + moveBy
          in [W_X desiredX, W_Y desiredY]
          end
      | JUMPING jumped =>
          if jumped + moveBy > jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let val newYAxis = FLOATING 0
            in [W_X desiredX, W_Y_AXIS newYAxis]
            end
          else
            (* jump *)
            let
              val newJumped = jumped + moveBy
              val newYAxis = JUMPING newJumped
              val desiredY = y - moveBy
            in
              [W_X desiredX, W_Y desiredY, W_Y_AXIS newYAxis]
            end
    end

  fun getJumpPatches (player, upHeld, downHeld, acc) =
    let
      val {yAxis, jumpPressed, ...} = player
    in
      case (upHeld, downHeld) of
        (false, false) =>
          let
            val yAxis = defaultYAxis yAxis
            val jumpPressed = false
          in
            W_JUMP_PRESSED jumpPressed :: W_Y_AXIS yAxis :: acc
          end
      | (true, true) =>
          let val yAxis = defaultYAxis yAxis
          in W_Y_AXIS yAxis :: acc
          end
      | (true, false) =>
          let
            val yAxis = onJumpPressed (yAxis, jumpPressed)
            val jumpPressed = true
          in
            W_Y_AXIS yAxis :: W_JUMP_PRESSED jumpPressed :: acc
          end
      | (false, true) =>
          let
            val jumpPressed = false
            val yAxis = DROP_BELOW_PLATFORM
          in
            W_Y_AXIS yAxis :: W_JUMP_PRESSED jumpPressed :: acc
          end
    end

  fun getMainAttackPatches (prevAttack, attackHeld, mainAttackPressed) =
    case prevAttack of
      MAIN_NOT_ATTACKING =>
        if attackHeld andalso not mainAttackPressed then MAIN_ATTACKING 0
        else prevAttack
    | MAIN_ATTACKING amt =>
        if amt = mainAttackLimit then MAIN_NOT_ATTACKING
        else let val amt = amt + 1 in MAIN_ATTACKING amt end

  fun getInputPatches (player: player, input) =
    case #mainAttack player of
      MAIN_NOT_ATTACKING =>
        let
          val
            { x
            , y
            , yAxis
            , jumpPressed
            , facing
            , mainAttack
            , mainAttackPressed
            , ...
            } = player

          val {leftHeld, rightHeld, upHeld, downHeld, attackHeld} = input

          val xAxis = getXAxis (leftHeld, rightHeld)
          val facing = getFacing (facing, xAxis)
          val mainAttack =
            getMainAttackPatches (mainAttack, attackHeld, mainAttackPressed)

          val mainAttackPressed =
            case mainAttack of
              MAIN_ATTACKING _ => true
            | _ => attackHeld

          val acc =
            [ W_X_AXIS xAxis
            , W_FACING facing
            , W_MAIN_ATTACK mainAttack
            , W_MAIN_ATTACK_PRESSED mainAttackPressed
            ]
          val acc = getJumpPatches (player, upHeld, downHeld, acc)
        in
          acc
        end
    | MAIN_ATTACKING _ =>
        let
          val {mainAttack, mainAttackPressed, ...} = player
          val {attackHeld, ...} = input
          val mainAttack =
            getMainAttackPatches (mainAttack, attackHeld, mainAttackPressed)
          val mainAttackPressed =
            case mainAttack of
              MAIN_ATTACKING _ => true
            | _ => mainAttackPressed
        in
          [ W_X_AXIS STAY_STILL
          , W_MAIN_ATTACK mainAttack
          , W_MAIN_ATTACK_PRESSED mainAttackPressed
          ]
        end

  fun getRecoilPatches player =
    case #recoil player of
      NO_RECOIL => []
    | RECOIL_LEFT recoiled =>
        (* if player is recoiling, don't accept or adjust any input.
         * However, if player has reached the recoil limit, exit the recoil
         * state and accept input.
         * *)
        if recoiled = recoilLimit then
          [W_RECOIL NO_RECOIL]
        else
          let
            val {x, y, health, attacked, facing, xAxis, ...} = player
            (* difference between RECOIL_LEFT and RECOIL_RIGHT
             * is the direction player moves back in *)
            val x = x - 5

            val xAxis = STAY_STILL
            val yAxis = FALLING
            val jumpPressed = false
            val recoiled = recoiled + 1
            val recoil = RECOIL_LEFT recoiled
            val facing = getFacing (facing, xAxis)
          in
            [ W_X x
            , W_X_AXIS xAxis
            , W_Y_AXIS yAxis
            , W_JUMP_PRESSED jumpPressed
            , W_RECOIL recoil
            , W_FACING facing
            ]
          end
    | RECOIL_RIGHT recoiled =>
        if recoiled = recoilLimit then
          [W_RECOIL NO_RECOIL]
        else
          let
            val {x, y, health, attacked, facing, xAxis, ...} = player
            val x = x + 5

            val xAxis = STAY_STILL
            val yAxis = FALLING
            val jumpPressed = false
            val recoiled = recoiled + 1
            val recoil = RECOIL_RIGHT recoiled
            val facing = getFacing (facing, xAxis)
          in
            [ W_X x
            , W_X_AXIS xAxis
            , W_Y_AXIS yAxis
            , W_JUMP_PRESSED jumpPressed
            , W_RECOIL recoil
            , W_FACING facing
            ]
          end

  fun move (game: game_type, input) =
    let
      val player = #player game

      val patches = getRecoilPatches player
      val player = withPatches (player, patches)

      val player =
        (* we only accept and handle input if player is not recoiling *)
        case #recoil player of
          NO_RECOIL =>
            let val patches = getInputPatches (player, input)
            in withPatches (player, patches)
            end
        | _ => player

      val patches = getMovePatches player
      val player = withPatches (player, patches)

      val patches = getEnvironmentPatches (player, game)
    in
      withPatches (player, patches)
    end

  (* block is placeholder asset *)
  fun helpGetDrawVec (x, y, size, width, height, attacked, mainAttack) =
    case mainAttack of
      MAIN_NOT_ATTACKING =>
        (case attacked of
           NOT_ATTACKED =>
             Block.lerp (x, y, size, size, width, height, 0.5, 0.5, 0.5)
         | ATTACKED amt =>
             if amt mod 5 = 0 then
               Block.lerp (x, y, size, size, width, height, 0.9, 0.9, 0.9)
             else
               Block.lerp (x, y, size, size, width, height, 0.5, 0.5, 0.5))
    | MAIN_ATTACKING _ =>
        (case attacked of
           NOT_ATTACKED =>
             Block.lerp (x, y, size, size, width, height, 1.0, 0.5, 0.5)
         | ATTACKED amt =>
             if amt mod 5 = 0 then
               Block.lerp (x, y, size, size, width, height, 1.0, 0.9, 0.9)
             else
               Block.lerp (x, y, size, size, width, height, 1.0, 0.5, 0.5))

  fun getDrawVec (player: player, width, height) =
    let
      val {x, y, attacked, mainAttack, ...} = player
      val wratio = width / 1920.0
      val hratio = height / 1080.0
    in
      if wratio < hratio then
        let
          val scale = 1080.0 * wratio
          val yOffset =
            if height > scale then (height - scale) / 2.0
            else if height < scale then (scale - height) / 2.0
            else 0.0

          val x = Real32.fromInt x * wratio
          val y = Real32.fromInt y * wratio + yOffset

          val realSize = realSize * wratio
        in
          helpGetDrawVec (x, y, realSize, width, height, attacked, mainAttack)
        end
      else
        let
          val scale = 1920.0 * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0

          val x = Real32.fromInt x * hratio + xOffset
          val y = Real32.fromInt y * hratio

          val realSize = realSize * hratio
        in
          helpGetDrawVec (x, y, realSize, width, height, attacked, mainAttack)
        end
    end
end
