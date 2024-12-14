structure Player =
struct
  datatype y_axis = ON_GROUND | FALLING | JUMPING of int | FLOATING of int
  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  (* width/height *)
  val size = 35
  val realSize = 35.0

  val moveBy = 5
  val jumpLimit = 150
  val floatLimit = 3

  type t =
    { yAxis: y_axis
    , xAxis: x_axis
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    }

  (* placeholder *)
  val initial: t =
    { yAxis = JUMPING 0
    , xAxis = STAY_STILL
    , health = 3
    , x = 500
    , y = 500
    , jumpPressed = false
    }

  (* placeholder *)
  fun getVec ({x, y, ...}: t) =
    Block.lerp (x, y, realSize, realSize, 1920.0, 1080.0, 0.5, 0.5, 0.5)

  fun mkPlayer (health, xAxis, yAxis, x, y, jumpPressed) =
    { yAxis = yAxis
    , xAxis = xAxis
    , health = health
    , x = x
    , y = y
    , jumpPressed = jumpPressed
    }

  fun checkWalls (yAxis, xAxis, x, y, health, jumpPressed, lst) =
    let
      open QuadTree
    in
      case lst of
        (QUERY_ON_LEFT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} = Wall.getID wallID
            val newX = wallX + wallWidth
          in
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, tl)
          end
      | (QUERY_ON_RIGHT_SIDE, wallID) :: tl =>
          let
            val {x = wallX, width = wallWidth, ...} = Wall.getID wallID
            val newX = wallX - size
          in
            checkWalls (yAxis, xAxis, newX, y, health, jumpPressed, tl)
          end
      | (QUERY_ON_BOTTOM_SIDE, wallID) :: tl =>
          let
            val {y = wallY, ...} = Wall.getID wallID
            val newY = wallY - size
          in
            checkWalls (ON_GROUND, xAxis, x, newY, health, jumpPressed, tl)
          end
      | (QUERY_ON_TOP_SIDE, wallID) :: tl =>
          checkWalls (yAxis, xAxis, x, y, health, jumpPressed, tl)
      | [] => mkPlayer (health, xAxis, yAxis, x, y, jumpPressed)
    end

  fun helpMove (x, y, xAxis, yAxis, health, jumpPressed) =
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
          let
            val collisions = QuadTree.getCollisionSides
              (desiredX, y, size, size, 0, 0, 1920, 1080, 0, Wall.tree)
          in
            checkWalls
              (yAxis, xAxis, desiredX, y, health, jumpPressed, collisions)
          end
      | FLOATING floated =>
          let
            val collisions = QuadTree.getCollisionSides
              (desiredX, y, size, size, 0, 0, 1920, 1080, 0, Wall.tree)

            val yAxis =
              if floated = floatLimit then FALLING else FLOATING (floated + 1)
          in
            checkWalls
              (yAxis, xAxis, desiredX, y, health, jumpPressed, collisions)
          end
      | FALLING =>
          let
            val desiredY = y + moveBy
            val collisions = QuadTree.getCollisionSides
              (desiredX, desiredY, size, size, 0, 0, 1920, 1080, 0, Wall.tree)
          in
            checkWalls
              ( yAxis
              , xAxis
              , desiredX
              , desiredY
              , health
              , jumpPressed
              , collisions
              )
          end
      | JUMPING jumped =>
          if jumped + moveBy > jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let
              val collisions = QuadTree.getCollisionSides
                (desiredX, y, size, size, 0, 0, 1920, 1080, 0, Wall.tree)
            in
              checkWalls
                ( FLOATING 0
                , xAxis
                , desiredX
                , y
                , health
                , jumpPressed
                , collisions
                )
            end
          else
            (* jump *)
            let
              val newJumped = jumped + moveBy
              val yAxis = JUMPING newJumped
              val desiredY = y - moveBy

              val collisions = QuadTree.getCollisionSides
                (desiredX, desiredY, size, size, 0, 0, 1920, 1080, 0, Wall.tree)
            in
              checkWalls
                ( yAxis
                , xAxis
                , desiredX
                , desiredY
                , health
                , jumpPressed
                , collisions
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
      ON_GROUND => if jumpPressed then prevAxis else JUMPING 0
    | _ => prevAxis

  fun move ({x, y, yAxis, health, jumpPressed, ...}: t, input) =
    let
      val {leftHeld, rightHeld, upHeld, downHeld} = input
      val xAxis = getXAxis (leftHeld, rightHeld)
    in
      case (upHeld, downHeld) of
        (false, false) =>
          let
            val yAxis = defaultYAxis yAxis
            val jumpPressed = false
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed)
          end
      | (true, true) =>
          let
            val yAxis = defaultYAxis yAxis
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed)
          end
      | (true, false) =>
          let
            val yAxis = onJumpPressed (yAxis, jumpPressed)
            val jumpPressed = true
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed)
          end
      | (false, true) =>
          (* todo: should move down if on platform *)
          let
            val jumpPressed = false
          in
            helpMove (x, y, xAxis, yAxis, health, jumpPressed)
          end
    end
end
