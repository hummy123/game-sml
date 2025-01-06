structure Player =
struct
  open GameType

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
    , enemies
    , charge
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
    , enemies = enemies
    , charge = charge
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
        , enemies
        , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
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
            , enemies
            , charge
            )
      | W_ENEMIES enemies =>
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
            , enemies
            , charge
            )
      | W_CHARGE charge =>
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
            , enemies
            , charge
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

  val halfSize = 35 div 2
  val halfRealSize = 35.0 / 2.0

  val moveBy = 5

  (* defeated enemy constants *)
  val defeatedPi = Real32.Math.pi / 180.0
  val defeatedSize = 7.0
  val defeatedDistance = 13.0

  (* timing variables; always start at 0, 
   * and revert to default state when limit is hit *)
  val jumpLimit = 150
  val floatLimit = 3
  val recoilLimit = 15
  val attackedLimit = 55
  val maxCharge = 60

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

  fun getMainAttackPatches (attackHeld, chargeHeld, charge) =
    if attackHeld andalso charge > 0 then MAIN_ATTACKING
    else if chargeHeld andalso not attackHeld then MAIN_CHARGING
    else MAIN_NOT_ATTACKING

  fun getInputPatches (player: player, input) =
    let
      val
        { x
        , y
        , yAxis
        , jumpPressed
        , facing
        , mainAttack
        , mainAttackPressed
        , charge
        , ...
        } = player

      val {leftHeld, rightHeld, upHeld, downHeld, attackHeld, chargeHeld} =
        input

      val xAxis = getXAxis (leftHeld, rightHeld)
      val facing = getFacing (facing, xAxis)
      val mainAttack = getMainAttackPatches (attackHeld, chargeHeld, charge)

      val charge =
        case mainAttack of
          MAIN_CHARGING => Int.min (charge + 1, maxCharge)
        | MAIN_ATTACKING => Int.max (charge - 1, 0)
        | MAIN_NOT_ATTACKING => charge

      val acc =
        [ W_X_AXIS xAxis
        , W_FACING facing
        , W_MAIN_ATTACK mainAttack
        , W_CHARGE charge
        ]
      val acc = getJumpPatches (player, upHeld, downHeld, acc)
    in
      acc
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

  fun runPhysicsAndInput (game: game_type, input) =
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

      val player =
        let
          val e = #enemies player
          val e = Vector.map (fn {angle} => 
          {
            angle =
              if angle < 360 then angle + 5 else 0
          })
           e
          val patches = [W_ENEMIES e]
        in
          withPatches (player, patches)
        end

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
    | MAIN_ATTACKING =>
        (case attacked of
           NOT_ATTACKED =>
             Block.lerp (x, y, size, size, width, height, 1.0, 0.5, 0.5)
         | ATTACKED amt =>
             if amt mod 5 = 0 then
               Block.lerp (x, y, size, size, width, height, 1.0, 0.9, 0.9)
             else
               Block.lerp (x, y, size, size, width, height, 1.0, 0.5, 0.5))
    | MAIN_CHARGING =>
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

  fun getFieldVec (player: player, width, height) =
    case #mainAttack player of
      MAIN_NOT_ATTACKING => Vector.fromList []
    | _ =>
        let
          val {x, y, ...} = player
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

              val x = (Real32.fromInt x - halfRealSize) * wratio
              val y = (Real32.fromInt y - halfRealSize) * wratio + yOffset

              val realSize = (realSize * 2.0) * wratio

              val {charge, ...} = player
              val alpha = Real32.fromInt charge / 60.0
            in
              Field.lerp
                (x, y, realSize, realSize, width, height, 0.7, 0.7, 1.0, alpha)
            end
          else
            let
              val scale = 1920.0 * hratio
              val xOffset =
                if width > scale then (width - scale) / 2.0
                else if width < scale then (scale - width) / 2.0
                else 0.0

              val x = (Real32.fromInt x - halfRealSize) * hratio + xOffset
              val y = (Real32.fromInt y - halfRealSize) * hratio

              val realSize = (realSize * 2.0) * hratio

              val {charge, ...} = player
              val alpha = Real32.fromInt charge / 60.0
            in
              Field.lerp
                (x, y, realSize, realSize, width, height, 0.7, 0.7, 1.0, alpha)
            end
        end

  fun degreesToRadians degrees = Real32.fromInt degrees * defeatedPi

  fun helpGetPelletVec
    ( playerX
    , playerY
    , pos
    , enemies
    , width
    , height
    , ratio
    , xOffset
    , yOffset
    , acc
    ) =
    if pos = Vector.length enemies then
      Vector.concat acc
    else
      let
        val {angle} = Vector.sub (enemies, pos)
        (* convert degrees to radians *)
        val angle = degreesToRadians angle

        (* calculate pellet's x and y *)
        val pelletX = ((Real32.Math.cos angle) * defeatedDistance) + playerX
        val pelletX = pelletX * ratio + xOffset

        val pelletY = ((Real32.Math.sin angle) * defeatedDistance) + playerY
        val pelletY = pelletY * ratio + yOffset

        val vec = Field.lerp
          ( pelletX
          , pelletY
          , defeatedSize
          , defeatedSize
          , width
          , height
          , 0.3
          , 0.9
          , 0.3
          , 1.0
          )
        val acc = vec :: acc
      in
        helpGetPelletVec
          ( playerX
          , playerY
          , pos + 1
          , enemies
          , width
          , height
          , ratio
          , xOffset
          , yOffset
          , acc
          )
      end

  fun getPelletVec (player: player, width, height) =
    if Vector.length (#enemies player) = 0 then
      Vector.fromList []
    else
      let
        val {x, y, enemies, ...} = player

        (* get centre (x, y) coordinates of player *)
        val diff = halfRealSize - (defeatedSize / 2.0)
        val x = Real32.fromInt x + diff
        val y = Real32.fromInt y + diff

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
          in 
            helpGetPelletVec
              (x, y, 0, enemies, width, height, wratio, 0.0, yOffset, [])
          end
        else
          let
            val scale = 1920.0 * hratio
            val xOffset =
              if width > scale then (width - scale) / 2.0
              else if width < scale then (scale - width) / 2.0
              else 0.0
          in
            helpGetPelletVec
              (x, y, 0, enemies, width, height, hratio, xOffset, 0.0, [])
          end
      end
end
