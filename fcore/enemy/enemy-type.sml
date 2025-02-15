signature ENEMY_TYPE =
sig
  datatype variant = PATROL_SLIME | FOLLOW_SLIME | STRAIGHT_BAT

  datatype bat_dir_y = UP | DOWN

  type enemy =
    { id: int
    , health: int
    , x: int
    , y: int
    , xAxis: EntityType.x_axis
    , yAxis: EntityType.y_axis
    , variant: variant
    , platID: int
    , nextPlatID: int
    , batRest: int
    , batDirY: bat_dir_y
    , batMaxY: int
    , batMinY: int
    , facing: EntityType.facing
    }

  type falling_enemy = {x: int, y: int, variant: variant}

  datatype shoot_x_axis = SHOOT_LEFT | SHOOT_RIGHT | NO_SHOOT_X
  datatype shoot_y_axis = SHOOT_UP | SHOOT_DOWN | NO_SHOOT_Y
end

structure EnemyType: ENEMY_TYPE =
struct
  datatype variant = PATROL_SLIME | FOLLOW_SLIME | STRAIGHT_BAT

  datatype bat_dir_y = UP | DOWN

  type enemy =
    { id: int
    , health: int
    , x: int
    , y: int
    , xAxis: EntityType.x_axis
    , yAxis: EntityType.y_axis
    , variant: variant
    , platID: int
    , nextPlatID: int
    , batRest: int
    , batDirY: bat_dir_y
    , batMaxY: int
    , batMinY: int
    , facing: EntityType.facing
    }

  type falling_enemy = {x: int, y: int, variant: variant}

  datatype shoot_x_axis = SHOOT_LEFT | SHOOT_RIGHT | NO_SHOOT_X
  datatype shoot_y_axis = SHOOT_UP | SHOOT_DOWN | NO_SHOOT_Y
end
