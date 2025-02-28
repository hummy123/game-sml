structure Player =
struct
  open PlayerPatch
  open EntityType
  open PlayerType

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

  fun getJumpPatches (player, jumpHeld, downHeld, acc) =
    let
      val {yAxis, jumpPressed, ...} = player
    in
      case (jumpHeld, downHeld) of
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

  (* called only when player has no projectiles or was not previously attacking *)
  fun helpGetMainAttackPatches (attackHeld, mainAttackPressed, charge, acc) =
    let
      val attack =
        if attackHeld andalso not mainAttackPressed then
          MAIN_ATTACKING 1
        else
          MAIN_NOT_ATTACKING
    in
      W_MAIN_ATTACK_PRESSED (attackHeld andalso mainAttackPressed)
      :: W_MAIN_ATTACK attack :: acc
    end

  fun degreesToRadians degrees = Real32.fromInt degrees * Constants.projectilePi

  fun defeatedEnemiesToProjectiles
    (pos, defeteadEnemies, player as {x, y, facing, ...}, acc) =
    if pos = Vector.length defeteadEnemies then
      Vector.fromList acc
    else
      let
        val halfProjectileSize = Constants.projectileSize / 2.0
        val diffX = Constants.halfPlayerWidthReal - halfProjectileSize
        val diffY = Constants.halfPlayerHeightReal - halfProjectileSize
        val x = Real32.fromInt x + diffX
        val y = Real32.fromInt y + diffY

        val {angle} = Vector.sub (defeteadEnemies, pos)
        val angle = degreesToRadians angle

        val x = ((Real32.Math.cos angle) * Constants.projectileDistance) + x
        val y = ((Real32.Math.sin angle) * Constants.projectileDistance) + y

        val x = Real32.toInt IEEEReal.TO_NEAREST x
        val y = Real32.toInt IEEEReal.TO_NEAREST y

        val acc = {x = x, y = y, facing = facing} :: acc
      in
        defeatedEnemiesToProjectiles (pos + 1, defeteadEnemies, player, acc)
      end

  fun getThrowPatches (defeteadEnemies, projectiles, player, acc) =
    let
      val newProjectiles =
        defeatedEnemiesToProjectiles (0, defeteadEnemies, player, [])

      (* concatenate new projectiles with previous projectiles *)
      val allProjectiles = Vector.concat [newProjectiles, projectiles]

      (* remove defeated enemies from player record *)
      val enemies = Vector.fromList []
    in
      W_MAIN_ATTACK MAIN_THROWING :: W_PROJECTILES allProjectiles
      :: W_ENEMIES enemies :: acc
    end

  fun getMainAttackPatches
    ( prevAttack
    , defeteadEnemies
    , projectiles
    , attackHeld
    , charge
    , player
    , acc
    , mainAttackPressed
    ) =
    case prevAttack of
      MAIN_NOT_ATTACKING =>
        if
          attackHeld andalso not mainAttackPressed
          andalso Vector.length defeteadEnemies > 0
        then
          (* shoot projectiles if player was not attacking previously, 
           * and there is more than one enemy *)
          getThrowPatches (defeteadEnemies, projectiles, player, acc)
        else
          helpGetMainAttackPatches (attackHeld, mainAttackPressed, charge, acc)
    | MAIN_ATTACKING amt =>
        let
          val acc =
            if amt = Constants.mainAttackLimit then
              W_MAIN_ATTACK MAIN_NOT_ATTACKING :: acc
            else
              let
                val amt = amt + 1
              in
                W_MAIN_ATTACK (MAIN_ATTACKING amt) :: acc
              end
        in
          W_MAIN_ATTACK_PRESSED true :: acc
        end
    | MAIN_THROWING =>
        if attackHeld then
          acc
        else
          helpGetMainAttackPatches (attackHeld, mainAttackPressed, charge, acc)

  fun getInputPatches (player: player, input: FrameInputType.t) =
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
        , enemies
        , projectiles
        , ...
        } = player

      val {leftHeld, rightHeld, upHeld, downHeld, attackHeld, jumpHeld, ...} =
        input

      val xAxis = getXAxis (leftHeld, rightHeld)
      val facing = getFacing (facing, xAxis)

      val charge = (* todo: rework charge *) charge

      val acc = [W_X_AXIS xAxis, W_FACING facing, W_CHARGE charge]

      val acc = getMainAttackPatches
        ( mainAttack
        , enemies
        , projectiles
        , attackHeld
        , charge
        , player
        , acc
        , mainAttackPressed
        )

      val acc = getJumpPatches (player, jumpHeld, downHeld, acc)
    in
      acc
    end

  fun getRecoilPatches (player: player, patches) =
    case #recoil player of
      NO_RECOIL => patches
    | RECOIL_LEFT recoiled =>
        (* if player is recoiling, don't accept or adjust any input.
         * However, if player has reached the recoil limit, exit the recoil
         * state and accept input.
         * *)
        if recoiled = Constants.recoilLimit then
          W_RECOIL NO_RECOIL :: patches
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
            W_X x :: W_X_AXIS xAxis :: W_Y_AXIS yAxis
            :: W_JUMP_PRESSED jumpPressed :: W_RECOIL recoil :: W_FACING facing
            :: patches
          end
    | RECOIL_RIGHT recoiled =>
        if recoiled = Constants.recoilLimit then
          W_RECOIL NO_RECOIL :: patches
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
            W_X x :: W_X_AXIS xAxis :: W_Y_AXIS yAxis
            :: W_JUMP_PRESSED jumpPressed :: W_RECOIL recoil :: W_FACING facing
            :: patches
          end

  fun helpMoveProjectiles (pos, projectiles, acc) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val {x, y, facing} = Vector.sub (projectiles, pos)
      in
        if x <= 0 orelse x >= Constants.worldWidth then
          (* filter out since projectile is not visible *)
          helpMoveProjectiles (pos - 1, projectiles, acc)
        else
          let
            val x =
              case facing of
                FACING_LEFT => x - Constants.moveProjectileBy
              | FACING_RIGHT => x + Constants.moveProjectileBy

            val newTile = {x = x, y = y, facing = facing}
            val acc = newTile :: acc
          in
            helpMoveProjectiles (pos - 1, projectiles, acc)
          end
      end

  fun getProjectilePatches ({projectiles, ...}: player) =
    let
      val newProjectiles = helpMoveProjectiles
        (Vector.length projectiles - 1, projectiles, [])
    in
      [W_PROJECTILES newProjectiles]
    end

  structure FoldEnemies =
    MakeQuadTreeFold
      (struct
         type env = EnemyMap.t * player
         type state = PlayerPatch.player_patch list

         fun getEnemyRecoilPatches (player, playerOnRight, acc) =
           if playerOnRight then
             let
               val newRecoil = RECOIL_RIGHT 0
               val newAttacked = ATTACKED 0
             in
               W_RECOIL newRecoil :: W_ATTACKED newAttacked
               :: W_FACING FACING_LEFT :: W_Y_AXIS FALLING
               :: W_X_AXIS STAY_STILL :: acc
             end
           else
             let
               val newRecoil = RECOIL_LEFT 0
               val newAttacked = ATTACKED 0
             in
               W_RECOIL newRecoil :: W_ATTACKED newAttacked
               :: W_FACING FACING_RIGHT :: W_Y_AXIS FALLING
               :: W_X_AXIS STAY_STILL :: acc
             end

         fun fold (enemyID, (enemies, player: player), patches) =
           let
             val playerOnRight =
               (* check if collision is closer to left side of enemy or right
                * and then chose appropriate direction to recoil in *)
               let
                 val {x, ...} = player
                 val pFinishX = x + Constants.playerWidth
                 val pHalfW = Constants.playerWidth div 2
                 val pCentreX = x + pHalfW
               in
                 case EnemyMap.get (enemyID, enemies) of
                   SOME {x = ex, y = ey, ...} =>
                     let
                       val eFinishX = ex + Constants.enemySize
                       val eHalfW = Constants.enemySize div 2
                       val eCentreX = ex + eHalfW
                     in
                       eCentreX < pCentreX
                     end
                 | NONE => false
               end

             val patches =
               getEnemyRecoilPatches (player, playerOnRight, patches)
           in
             W_ATTACKED (ATTACKED 0) :: patches
           end
       end)

  structure AttackEnemies =
    MakeQuadTreeFold
      (struct
         type env = unit
         type state = defeated_enemies list

         fun fold (_, (), defeatedList) = {angle = 1} :: defeatedList
       end)

  fun runPhysicsAndInput (game: LevelType.level_type, input) =
    let
      val player = #player game

      val oldAnimTimer = #animTimer player
      val oldXAxis = #xAxis player
      val oldYAxis = #yAxis player

      val patches = getProjectilePatches player
      val patches = getRecoilPatches (player, patches)
      val player = PlayerPatch.withPatches (player, patches)

      val patches =
        (* we only accept and handle input if player is not recoiling.
         * It's important to apply the recoil patches after handling input
         * because we want to act on the latest recoil state straight away. *)
        case #recoil player of
          NO_RECOIL => getInputPatches (player, input)
        | _ => []

      val patches =
        (* control timer for how long player should be immune to damage
         * after being attacked *)
        case #attacked player of
          ATTACKED amt =>
            if amt >= Constants.attackedLimit then
              W_ATTACKED NOT_ATTACKED :: patches
            else
              W_ATTACKED (ATTACKED (amt + 1)) :: patches
        | _ => patches

      (* animate projectiles *)
      val player =
        let
          val e = #enemies player
          val e =
            Vector.map
              (fn {angle} => {angle = if angle < 360 then angle + 5 else 0}) e
          val patches = W_ENEMIES e :: patches
        in
          PlayerPatch.withPatches (player, patches)
        end

      val patches = PlayerPhysics.getPhysicsPatches player
      val player = PlayerPatch.withPatches (player, patches)

      val {walls, wallTree, platforms, platformTree, ...} = game
      val patches = PlayerPhysics.getEnvironmentPatches
        (player, walls, wallTree, platforms, platformTree)

      val player = PlayerPatch.withPatches (player, patches)

      val newXAxis = #xAxis player
      val newYAxis = #yAxis player
    in
      if oldYAxis = DROP_BELOW_PLATFORM andalso newYAxis = DROP_BELOW_PLATFORM then
        PlayerPatch.withPatch (player, W_ANIM_TIMER (oldAnimTimer + 1))
      else if
        oldYAxis = ON_GROUND andalso newYAxis = ON_GROUND
      then
        if oldXAxis = MOVE_RIGHT andalso newXAxis = MOVE_RIGHT then
          (* update move-right animation *)
          PlayerPatch.withPatch (player, W_ANIM_TIMER (oldAnimTimer + 1))
        else if oldXAxis = MOVE_LEFT andalso newXAxis = MOVE_LEFT then
          (* update move-left animation *)
          PlayerPatch.withPatch (player, W_ANIM_TIMER (oldAnimTimer + 1))
        else
          PlayerPatch.withPatch (player, W_ANIM_TIMER 0)
      else
        PlayerPatch.withPatch (player, W_ANIM_TIMER 0)
    end

  (* player reaction to collisions with enemies.
   * We only detect collisions if player is not in invincibility period
   * after being previously attacked. *)
  fun checkEnemyCollisions (player: PlayerType.player, enemies, enemyTree) =
    case #attacked player of
      ATTACKED _ => player
    | _ =>
        let
          val {x, y, ...} = player
          val ew = Constants.playerWidth
          val eh = Constants.playerHeight
          val env = (enemies, player)
          val state = []
          val patches = FoldEnemies.foldRegion
            (x, y, ew, eh, env, state, enemyTree)
        in
          PlayerPatch.withPatches (player, patches)
        end

  (*** DRAWING FUNCTIONS ***)
  fun helpGetWhipVec (tlx, tly, ratio, xOffset, yOffset, pos, boxes, width, height, acc) =
    if pos = Vector.length boxes then
      Vector.concat acc
    else
      let
        val {x, y} = Vector.sub (boxes, pos)
        val x = tlx + x
        val y = tly + y

        val x = Real32.fromInt x * ratio + xOffset
        val y = Real32.fromInt y * ratio + yOffset

        val size = Whip.sizeReal
        val acc = Box.lerp (x, y, size, size, width, height) :: acc
      in
        helpGetWhipVec (tlx, tly, ratio, xOffset, yOffset, pos + 1, boxes, width, height, acc)
      end

  fun getFieldVec (player: player, width, height) =
    case #mainAttack player of
      MAIN_ATTACKING amt =>
        let
          val frame = amt div 2
          val {x, y, facing, ...} = player
          val wratio = width / Constants.worldWidthReal
          val hratio = height / Constants.worldHeightReal
        in
          if wratio < hratio then
            let
              val scale = Constants.worldHeightReal * wratio
              val yOffset =
                if height > scale then (height - scale) / 2.0
                else if height < scale then (scale - height) / 2.0
                else 0.0

              val boxes =
                case facing of
                  FACING_RIGHT =>
                    Vector.sub (Whip.rightFrames, frame)
                | FACING_LEFT =>
                    (* todo: change to leftFrames once that is implemented *)
                    Vector.sub (Whip.rightFrames, frame)
            in
              helpGetWhipVec (x, y, wratio, 0.0, yOffset, 0, boxes, width, height, [])
            end
          else
            let
              val scale = Constants.worldWidthReal * hratio
              val xOffset =
                if width > scale then (width - scale) / 2.0
                else if width < scale then (scale - width) / 2.0
                else 0.0

              val boxes =
                case facing of
                  FACING_RIGHT =>
                    Vector.sub (Whip.rightFrames, frame)
                | FACING_LEFT =>
                    (* todo: change to leftFrames once that is implemented *)
                    Vector.sub (Whip.rightFrames, frame)
            in
              helpGetWhipVec (x, y, hratio, xOffset, 0.0, 0, boxes, width, height, [])
            end
        end
    | _ => Vector.fromList []

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
        val pelletX =
          ((Real32.Math.cos angle) * Constants.projectileDistance) + playerX
        val pelletX = pelletX * ratio + xOffset

        val pelletY =
          ((Real32.Math.sin angle) * Constants.projectileDistance) + playerY
        val pelletY = pelletY * ratio + yOffset

        val defeatedSize = Constants.projectileSize * ratio

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
        val halfProjectileSize = Constants.projectileSize / 2.0
        val diffX = Constants.halfPlayerWidthReal - halfProjectileSize
        val diffY = Constants.halfPlayerHeightReal - halfProjectileSize
        val x = Real32.fromInt x + diffX
        val y = Real32.fromInt y + diffY

        val wratio = width / Constants.worldWidthReal
        val hratio = height / Constants.worldHeightReal
      in
        if wratio < hratio then
          let
            val scale = Constants.worldHeightReal * wratio
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
            val scale = Constants.worldWidthReal * hratio
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
