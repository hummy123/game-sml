structure Player =
struct
  open GameType

  (* width/height *)
  val size = 35
  val realSize = 35.0

  val moveBy = 5
  val jumpLimit = 150
  val floatLimit = 3

  fun mkPlayer (health, xAxis, yAxis, x, y, jumpPressed) =
    { yAxis = yAxis
    , xAxis = xAxis
    , health = health
    , x = x
    , y = y
    , jumpPressed = jumpPressed
    }

  fun checkWalls (yAxis, xAxis, x, y, health, jumpPressed, lst, game: game_type) =
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
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, tl, game)
          end
      | (QUERY_ON_RIGHT_SIDE, wallID) :: tl =>
          let
            val {walls, ...} = game
            val {x = wallX, width = wallWidth, ...} =
              Vector.sub (walls, wallID - 1)

            val newX = wallX - size
          in
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, tl, game)
          end
      | (QUERY_ON_BOTTOM_SIDE, wallID) :: tl =>
          let
            val {walls, ...} = game
            val {y = wallY, ...} = Vector.sub (walls, wallID - 1)

            val newY = wallY - size
          in
            checkWalls
              (ON_GROUND, xAxis, x, newY, health, jumpPressed, tl, game)
          end
      | (QUERY_ON_TOP_SIDE, wallID) :: tl =>
          checkWalls (yAxis, xAxis, x, y, health, jumpPressed, tl, game)
      | [] =>
          mkPlayer (health, xAxis, yAxis, x, y, jumpPressed)
    end

  fun helpCheckPlatforms
    ( yAxis, xAxis, x, y, health
    , jumpPressed, platList, wallList, game
    ) =
    let
      open QuadTree
    in
      case platList of
        platID :: tl =>
          (case yAxis of
            DROP_BELOW_PLATFORM =>
              helpCheckPlatforms
                (yAxis, xAxis, x, y, health, jumpPressed, tl, wallList, game)
          | _ =>
          let
            val {platforms, ...} = game
            val {y = platY, ...} = Vector.sub (platforms, platID - 1)

            val newY = platY - size
          in
            helpCheckPlatforms
              (ON_GROUND, xAxis, x, newY, health, jumpPressed, tl, wallList, game)
          end)
      | [] => 
          checkWalls (yAxis, xAxis, x, y, health, jumpPressed, wallList, game)
    end

  (*** MLTON STRANGE TYPES ERROR:
   *** Trigger by deleting the longer  `checkPlatforms` function
   *** and uncommenting the function of the same name that raises Match.

  fun checkPlatforms (yAxis, xAxis, x, y, health, jumpPressed, game) =
    raise Match

   *** *)

  fun checkPlatforms (yAxis, xAxis, x, y, health, jumpPressed, game) =
    let
      val {wallTree, platformTree, ...} = game
      val platCollisions = QuadTree.getCollisionsBelow
        (y, y, size, size, 0, 0, 1920, 1080, 0, wallTree)

      val wallCollisions = QuadTree.getCollisionSides
        (y, y, size, size, 0, 0, 1920, 1080, 0, wallTree)
    in
      helpCheckPlatforms
        ( yAxis, xAxis, x, y, health, jumpPressed
        , platCollisions, wallCollisions, game
        )
    end

  fun helpMove (x, y, xAxis, yAxis, health, jumpPressed, game: game_type) =
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
          checkPlatforms
            (yAxis, xAxis, desiredX, y, health, jumpPressed, game)
      | FLOATING floated =>
          let
            val yAxis =
              if floated = floatLimit then FALLING else FLOATING (floated + 1)
          in
            checkPlatforms
              (yAxis, xAxis, desiredX, y, health, jumpPressed, game)
          end
      | FALLING =>
          let
            val desiredY = y + moveBy
          in
            checkPlatforms
              (yAxis, xAxis, desiredX, desiredY, health, jumpPressed, game)
          end
      | DROP_BELOW_PLATFORM =>
          let
            val desiredY = y + moveBy
          in
            checkPlatforms
              (yAxis, xAxis, desiredX, desiredY, health, jumpPressed, game)
          end
      | JUMPING jumped =>
          if jumped + moveBy > jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let
              val newYAxis = FLOATING 0
            in
              checkPlatforms
                (newYAxis, xAxis, desiredX, y, health, jumpPressed, game)
            end
          else
            (* jump *)
            let
              val newJumped = jumped + moveBy
              val newYAxis = JUMPING newJumped
              val desiredY = y - moveBy
            in
              checkPlatforms
                (newYAxis, xAxis, desiredX, desiredY, health, jumpPressed, game)
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
      ON_GROUND => if jumpPressed then prevAxis else JUMPING 0
    | _ => prevAxis

  fun move (game: game_type, input) =
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
            helpMove (x, y, xAxis, yAxis, health, jumpPressed, game)
          end
      | (true, true) =>
          let val yAxis = defaultYAxis yAxis
          in helpMove (x, y, xAxis, yAxis, health, jumpPressed, game)
          end
      | (true, false) =>
          let
            val yAxis = onJumpPressed (yAxis, jumpPressed)
            val jumpPressed = true
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed, game)
          end
      | (false, true) =>
          (* todo: should move down if on platform *)
          let val jumpPressed = false
          in helpMove (x, y, xAxis, yAxis, health, jumpPressed, game)
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
