signature ENEMY_PATCH =
sig
  datatype enemy_patch =
    W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_X_AXIS of EntityType.x_axis
  | W_Y_AXIS of EntityType.y_axis
  | W_PLAT_ID of int
  | W_NEXT_PLAT_ID of int
  | W_BAT_REST of int
  | W_BAT_MAX_Y of int
  | W_BAT_MIN_Y of int
  | W_BAT_DIR_Y of EnemyType.bat_dir_y
  | W_FACING of EntityType.facing
  | W_SHIELD_ON of bool

  val withPatch: EnemyType.enemy * enemy_patch -> EnemyType.enemy

  val withPatches: EnemyType.enemy * enemy_patch list -> EnemyType.enemy
end

structure EnemyPatch: ENEMY_PATCH =
struct
  datatype enemy_patch =
    W_HEALTH of int
  | W_X of int
  | W_Y of int
  | W_X_AXIS of EntityType.x_axis
  | W_Y_AXIS of EntityType.y_axis
  | W_PLAT_ID of int
  | W_NEXT_PLAT_ID of int
  | W_BAT_REST of int
  | W_BAT_MAX_Y of int
  | W_BAT_MIN_Y of int
  | W_BAT_DIR_Y of EnemyType.bat_dir_y
  | W_FACING of EntityType.facing
  | W_SHIELD_ON of bool

  fun mkEnemy
    ( id
    , health
    , x
    , y
    , xAxis
    , yAxis
    , variant
    , platID
    , nextPlatID
    , batRest
    , batDirY
    , batMaxY
    , batMinY
    , facing
    , shieldOn
    ) =
    { id = id
    , health = health
    , x = x
    , y = y
    , xAxis = xAxis
    , yAxis = yAxis
    , variant = variant
    , platID = platID
    , nextPlatID = nextPlatID
    , batRest = batRest
    , batDirY = batDirY
    , batMaxY = batMaxY
    , batMinY = batMinY
    , facing = facing
    , shieldOn = shieldOn
    }

  fun withPatch (enemy, patch) =
    let
      val
        { id
        , health
        , x
        , y
        , xAxis
        , yAxis
        , variant
        , platID
        , nextPlatID
        , batRest
        , batDirY
        , batMaxY
        , batMinY
        , facing
        , shieldOn
        } = enemy
    in
      case patch of
        W_HEALTH health =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_X x =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_X_AXIS xAxis =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_Y y =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_Y_AXIS yAxis =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_PLAT_ID platID =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_NEXT_PLAT_ID nextPlatID =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_BAT_REST batRest =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_BAT_MAX_Y batMaxY =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_BAT_MIN_Y batMinY =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_BAT_DIR_Y batDirY =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_FACING facing =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
      | W_SHIELD_ON shieldOn =>
          mkEnemy
            ( id
            , health
            , x
            , y
            , xAxis
            , yAxis
            , variant
            , platID
            , nextPlatID
            , batRest
            , batDirY
            , batMaxY
            , batMinY
            , facing
            , shieldOn
            )
    end

  fun withPatches (enemy, lst) =
    case lst of
      hd :: tl =>
        let val enemy = withPatch (enemy, hd)
        in withPatches (enemy, tl)
        end
    | [] => enemy
end
