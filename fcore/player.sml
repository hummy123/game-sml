structure Player =
struct
  datatype y_axis = ON_GROUND | FALLING | JUMPING of int
  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  (* width/height *)
  val size = 35

  val moveBy = 5
  val jumpLimit = 55

  type t = {yAxis: y_axis, xAxis: x_axis, health: int, x: int, y: int}

  fun mkPlayer (health, xAxis, yAxis, x, y) =
    {yAxis = yAxis, xAxis = xAxis, health = health, x = x, y = y}

  fun move (player: t) =
    let
      val {yAxis, xAxis, x, y, health} = player

      (* todo: check for wall and platform collisions 
       * in case analysis for both axis
       * *)
      val x =
        case xAxis of
          MOVE_LEFT =>
            (* todo: check if we are trying to move left 
             * even though player is against wall.
             * In that case, keep same action (it is a sign for us to animate),
             * but don't actually move leftwards. *)
            x - moveBy
        | MOVE_RIGHT => (* todo: check against wall *) x + moveBy
        | STAY_STILL => x
    in
      case yAxis of
        JUMPING jumped =>
          (* check if we hit jump limit; 
           * if we did, change to falling case. 
           * *)
          if jumped + moveBy <= jumpLimit then
            let
              val jumped = jumped + moveBy
              val yAxis = JUMPING jumped
              val y = y + moveBy
            in
              mkPlayer (health, xAxis, yAxis, x, y)
            end
          else
            mkPlayer (health, xAxis, FALLING, x, y)
      | FALLING =>
          (* todo: keep decrementing and falling down 
           * until we hit ground or platform 
           * *)
          mkPlayer (health, xAxis, yAxis, x, y - moveBy)
      | ON_GROUND => mkPlayer (health, xAxis, yAxis, x, y)
    end
end
