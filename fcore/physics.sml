signature PHYSICS_INPUT =
sig
  type t
  type patch

  (* constants for physics *)
  val moveBy: int
  val floatLimit: int
  val jumpLimit: int

  (* destructuring functions *)
  val getX: t -> int
  val getY: t -> int
  val getXAxis: t -> GameType.x_axis
  val getYAxis: t -> GameType.y_axis

  val W_X: int -> patch
  val W_Y: int -> patch
  val W_Y_AXIS: GameType.y_axis -> patch
end

functor MakePhysics(Fn: PHYSICS_INPUT) =
struct
  open GameType

  fun getPatches input =
    let
      val x = Fn.getX input
      val y = Fn.getY input
      val xAxis = Fn.getXAxis input
      val yAxis = Fn.getYAxis input

      val desiredX =
        case xAxis of
          STAY_STILL => x
        | MOVE_LEFT => x - Fn.moveBy
        | MOVE_RIGHT => x + Fn.moveBy
    in
      case yAxis of
        ON_GROUND => [Fn.W_X desiredX]
      | FLOATING floated =>
          let
            val yAxis =
              if floated = Fn.floatLimit then FALLING
              else FLOATING (floated + 1)
          in
            [Fn.W_X desiredX, Fn.W_Y_AXIS yAxis]
          end
      | FALLING =>
          let val desiredY = y + Fn.moveBy
          in [Fn.W_X desiredX, Fn.W_Y desiredY]
          end
      | DROP_BELOW_PLATFORM =>
          let val desiredY = y + Fn.moveBy
          in [Fn.W_X desiredX, Fn.W_Y desiredY]
          end
      | JUMPING jumped =>
          if jumped + Fn.moveBy > Fn.jumpLimit then
            (* if we are above the jump limit, trigger a fall *)
            let val newYAxis = FLOATING 0
            in [Fn.W_X desiredX, Fn.W_Y_AXIS newYAxis]
            end
          else
            (* jump *)
            let
              val newJumped = jumped + Fn.moveBy
              val newYAxis = JUMPING newJumped
              val desiredY = y - Fn.moveBy
            in
              [Fn.W_X desiredX, Fn.W_Y desiredY, Fn.W_Y_AXIS newYAxis]
            end
    end
end

structure PlayerPhysics =
  MakePhysics
    (struct
       type t = GameType.player
       type patch = PlayerPatch.player_patch

       (* constants for physics *)
       val moveBy = Constants.movePlayerBy
       val floatLimit = Constants.floatLimit
       val jumpLimit = Constants.jumpLimit

       (* destructuring functions *)
       fun getX ({x, ...}: t) = x
       fun getY ({y, ...}: t) = y

       fun getXAxis ({xAxis, ...}: t) = xAxis
       fun getYAxis ({yAxis, ...}: t) = yAxis

       val W_X = PlayerPatch.W_X
       val W_Y = PlayerPatch.W_Y
       val W_Y_AXIS = PlayerPatch.W_Y_AXIS
     end)
