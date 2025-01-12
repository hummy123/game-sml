signature ENEMY_PATCH =
sig
  datatype enemy_patch =
    W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_X_AXIS of GameType.x_axis
  | W_Y_AXIS of GameType.y_axis

  val withPatch: GameType.enemy * enemy_patch -> GameType.enemy

  val withPatches: GameType.enemy * enemy_patch list -> GameType.enemy
end

structure EnemyPatch: ENEMY_PATCH =
struct
  datatype enemy_patch =
    W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_X_AXIS of GameType.x_axis
  | W_Y_AXIS of GameType.y_axis

  fun mkEnemy (id, health, x, y, xAxis, yAxis) =
    {id = id, health = health, x = x, y = y, xAxis = xAxis, yAxis = yAxis}

  fun withPatch (enemy, patch) =
    let
      val {id, health, x, y, xAxis, yAxis} = enemy
    in
      case patch of
        W_HEALTH health => mkEnemy (id, health, x, y, xAxis, yAxis)
      | W_X x => mkEnemy (id, health, x, y, xAxis, yAxis)
      | W_X_AXIS xAxis => mkEnemy (id, health, x, y, xAxis, yAxis)
      | W_Y y => mkEnemy (id, health, x, y, xAxis, yAxis)
      | W_Y_AXIS yAxis => mkEnemy (id, health, x, y, xAxis, yAxis)
    end

  fun withPatches (enemy, lst) =
    case lst of
      hd :: tl =>
        let val enemy = withPatch (enemy, hd)
        in withPatches (enemy, tl)
        end
    | [] => enemy
end
