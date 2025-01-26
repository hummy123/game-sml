signature ENEMY_PATCH =
sig
  datatype enemy_patch =
    W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_X_AXIS of GameType.x_axis
  | W_Y_AXIS of GameType.y_axis
  | W_PLAT_ID of int
  | W_NEXT_PLAT_ID of int

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
  | W_PLAT_ID of int
  | W_NEXT_PLAT_ID of int

  fun mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID) =
    { id = id
    , health = health
    , x = x
    , y = y
    , xAxis = xAxis
    , yAxis = yAxis
    , variant = variant
    , platID = platID
    , nextPlatID = nextPlatID
    }

  fun withPatch (enemy, patch) =
    let
      val {id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID} = enemy
    in
      case patch of
        W_HEALTH health =>
          mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_X x => mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_X_AXIS xAxis =>
          mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_Y y => mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_Y_AXIS yAxis =>
          mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_PLAT_ID platID =>
          mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
      | W_NEXT_PLAT_ID nextPlatID =>
          mkEnemy (id, health, x, y, xAxis, yAxis, variant, platID, nextPlatID)
    end

  fun withPatches (enemy, lst) =
    case lst of
      hd :: tl =>
        let val enemy = withPatch (enemy, hd)
        in withPatches (enemy, tl)
        end
    | [] => enemy
end
