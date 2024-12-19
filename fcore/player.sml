structure Player =
struct
  open GameType

  (* width/height *)
  val size = 35
  val realSize = 35.0

  val moveBy = 5

  val jumpLimit = 150
  val floatLimit = 3
  val recoilLimit = 15

  fun mkPlayer (health, xAxis, yAxis, x, y, jumpPressed, recoil) =
    { yAxis = yAxis
    , xAxis = xAxis
    , recoil = recoil
    , health = health
    , x = x
    , y = y
    , jumpPressed = jumpPressed
    }

  fun checkWalls (yAxis, xAxis, x, y, health, jumpPressed, recoil, lst, game: game_type) =
    let
      open QuadTree
    in
      case lst of
        (QUERY_ON_LEFT_SIDE, wallID) :: tl =>
          let
            val {walls, ...} = game
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX + wallWidth
          in
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, recoil, tl, game)
          end
      | (QUERY_ON_RIGHT_SIDE, wallID) :: tl =>
          let
            val {walls, ...} = game
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX - size
          in
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, recoil, tl, game)
          end
      | (QUERY_ON_BOTTOM_SIDE, wallID) :: tl =>
          let
            val {walls, ...} = game
            val {y = wallY, ...} = Vector.sub (walls, wallID - 1)

            val newY = wallY - size
          in
            checkWalls
              (ON_GROUND, xAxis, x, newY, health, jumpPressed, recoil, tl, game)
          end
      | (QUERY_ON_TOP_SIDE, wallID) :: tl =>
          checkWalls (yAxis, xAxis, x, y, health, jumpPressed, recoil, tl, game)
      | [] =>
          mkPlayer (health, xAxis, yAxis, x, y, jumpPressed, recoil)
    end

  fun helpCheckPlatforms
    ( yAxis, xAxis, x, y, health
    , jumpPressed, recoil
    , platList, wallList, game
    ) =
    let
      open QuadTree
    in
      case platList of
        platID :: tl =>
          (case yAxis of
            DROP_BELOW_PLATFORM =>
              (* pass through, allowing player to drop below the platform *)
              helpCheckPlatforms
                (yAxis, xAxis, x, y, health, jumpPressed, recoil, tl, wallList, game)
          | JUMPING _ =>
              (* pass through, allowing player to jump above the platform *)
              helpCheckPlatforms
                (yAxis, xAxis, x, y, health, jumpPressed, recoil, tl, wallList, game)
          | _ =>
            let
              (* default case: 
               * player will land on platform and stay on the ground there. *)

              (*** 
               *** cause of compiler error is here 
               *** The specific error is an error with optimising record representations.
               ***
               *** TO make the problem go away (at the cost of incorrectness),
               *** one can:
               *** 1. Delete the call to Vector.sub below
               *** 2. Change the `platY` value below (in `platY - size`)
               ***    to any constant integer (like 300 or 555).
               ***)

              val {platforms, ...} = game
              val {y = platY, ...} = Vector.sub (platforms, platID - 1)

              val newY = platY - size
            in
              helpCheckPlatforms
                (ON_GROUND, xAxis, x, newY, health, jumpPressed, recoil, tl, wallList, game)
            end)
      | [] => 
          checkWalls (yAxis, xAxis, x, y, health, jumpPressed, recoil, wallList, game)
    end

  fun checkEnemies 
    ( yAxis, xAxis, x, y, health, jumpPressed, recoil
    , enemyCollisions, platCollisions, wallCollisions, game
    ) =
    case enemyCollisions of
      id :: tl =>
        let
          val newRecoil =
            (* check if collision is closer to left side of enemy or right
             * and then chose appropriate direction to recoil in *)
            let
              val pFinishX = x + size
              val pHalfW = size div 2
              val pCentreX = x + pHalfW

              val {x = ex, y = ey, ...} = Vector.sub (#enemies game, id - 1)
              val eFinishX = ex + Enemy.size
              val eHalfW = Enemy.size div 2
              val eCentreX = ex + eHalfW
            in
              if eCentreX < pCentreX then
                RECOIL_RIGHT 0
              else
                RECOIL_LEFT 0
            end
        in
          checkEnemies
            ( FALLING, STAY_STILL, x, y, health, jumpPressed, newRecoil
            , tl, platCollisions, wallCollisions, game
            )
        end
    | [] =>
        helpCheckPlatforms
           ( yAxis, xAxis, x, y, health, jumpPressed, recoil
           , platCollisions, wallCollisions, game
           )

  fun checkCollisions (yAxis, xAxis, x, y, health, jumpPressed, recoil, game) =
    let
      val {wallTree, platformTree, enemyTree, ...} = game

      (* control flow is: check enemies -> check platforms -> check walls
       * but this is not visible in this function as everything is implemented
       * by tail call.
       * So, when one function hits the end of its collision list, 
       * it calls the next function at its tail. *)

      val platCollisions = QuadTree.getCollisionsBelow
        (x, y, size, size, 0, 0, 1920, 1080, 0, platformTree)

      val wallCollisions = QuadTree.getCollisionSides
        (x, y, size, size, 0, 0, 1920, 1080, 0, wallTree)
    in
      case recoil of
        NO_RECOIL =>
          let
            val enemyCollisions = QuadTree.getCollisions
              (x, y, size, size, 0, 0, 1920, 1080, 0, enemyTree)
          in
            checkEnemies
              ( yAxis, xAxis, x, y, health, jumpPressed, recoil
              , enemyCollisions, platCollisions, wallCollisions, game
              )
          end
      | _ =>
          helpCheckPlatforms
            ( yAxis, xAxis, x, y, health, jumpPressed, recoil
            , platCollisions, wallCollisions, game
            )
    end

  fun helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game) =
    let
      (* check against wall quad tree *)
      val desiredX =
        case xAxis of
          STAY_STILL => x
        | MOVE_LEFT => x - moveBy
        | MOVE_RIGHT => x + moveBy
    in
      case yAxis of
        ON_GROUND =>
          checkCollisions
            (yAxis, xAxis, desiredX, y, health, jumpPressed, recoil, game)
      | FLOATING floated =>
          let
            val yAxis =
              if floated = floatLimit then FALLING else FLOATING (floated + 1)
          in
            checkCollisions
              (yAxis, xAxis, desiredX, y, health, jumpPressed, recoil, game)
          end
      | FALLING =>
          let
            val desiredY = y + moveBy
          in
            checkCollisions
              (yAxis, xAxis, desiredX, desiredY, health, jumpPressed, recoil, game)
          end
      | DROP_BELOW_PLATFORM =>
          let
            val desiredY = y + moveBy
          in
            checkCollisions
              (yAxis, xAxis, desiredX, desiredY, health, jumpPressed, recoil, game)
          end
      | JUMPING jumped =>
          if jumped + moveBy > jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let
              val newYAxis = FLOATING 0
            in
              checkCollisions
                (newYAxis, xAxis, desiredX, y, health, jumpPressed, recoil, game)
            end
          else
            (* jump *)
            let
              val newJumped = jumped + moveBy
              val newYAxis = JUMPING newJumped
              val desiredY = y - moveBy
            in
              checkCollisions
                ( newYAxis, xAxis, desiredX, desiredY
                , health, jumpPressed, recoil, game
                )
            end
    end

  fun getXAxis (lh, rh) =
    case (lh, rh) of
      (false, false) => STAY_STILL
    | (false, true) => MOVE_RIGHT
    | (true, false) => MOVE_LEFT
    | (true, true) => STAY_STILL

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
    | _ => FALLING

  (* We want to prevent a double jump
   * or jumping while the player is falling
   * so we only switch to the JUMPING case if the player
   * is on the ground. *)
  fun onJumpPressed (prevAxis, jumpPressed) =
    case prevAxis of
      ON_GROUND => 
        if jumpPressed then 
          (* apply gravity *)
          FALLING 
        else 
          JUMPING 0
    | _ => prevAxis

  fun handleInput (game: game_type, input, recoil) =
    let
      val {x, y, yAxis, health, jumpPressed, ...} = #player game
      val {leftHeld, rightHeld, upHeld, downHeld} = input

      val xAxis = getXAxis (leftHeld, rightHeld)
    in
      case (upHeld, downHeld) of
        (false, false) =>
          let
            val yAxis = defaultYAxis yAxis
            val jumpPressed = false
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
          end
      | (true, true) =>
          let val yAxis = defaultYAxis yAxis
          in helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
          end
      | (true, false) =>
          let
            val yAxis = onJumpPressed (yAxis, jumpPressed)
            val jumpPressed = true
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
          end
      | (false, true) =>
          let 
            val jumpPressed = false
            val yAxis = DROP_BELOW_PLATFORM
          in 
            helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
          end
    end

  fun move (game: game_type, input) =
    let
      val player = #player game
      val recoil = #recoil player
    in
      case recoil of
        NO_RECOIL => handleInput (game, input, recoil)
      | RECOIL_LEFT recoiled =>
          (* if player is recoiling, don't accept or adjust any input.
           * However, if player has reached the recoil limit, exit the recoil
           * state and accept input.
           * *)
          if recoiled = recoilLimit then
            handleInput (game, input, NO_RECOIL)
          else
            let
              val {x, y, health, ...} = player
              (* difference between RECOIL_LEFT and RECOIL_RIGHT
               * is the direction player moves back in *)
              val x = x - 5

              val xAxis = STAY_STILL
              val yAxis = FALLING
              val jumpPressed = false
              val recoiled = recoiled + 1
              val recoil = RECOIL_LEFT recoiled
            in
              helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
            end
      | RECOIL_RIGHT recoiled =>
          if recoiled = recoilLimit then
            handleInput (game, input, NO_RECOIL)
          else
            let
              val {x, y, health, ...} = player
              val x = x + 5

              val xAxis = STAY_STILL
              val yAxis = FALLING
              val jumpPressed = false
              val recoiled = recoiled + 1
              val recoil = RECOIL_RIGHT recoiled
            in
              helpMove (x, y, xAxis, yAxis, health, jumpPressed, recoil, game)
            end
    end

  (* block is placeholder asset *)
  fun getDrawVec ({x, y, ...}: player, width, height) =
    let
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
          Block.lerp (x, y, realSize, realSize, width, height, 0.5, 0.5, 0.5)
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
          Block.lerp (x, y, realSize, realSize, width, height, 0.5, 0.5, 0.5)
        end
    end
end
