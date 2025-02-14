structure PlayerType =
struct
  datatype player_recoil = NO_RECOIL | RECOIL_LEFT of int | RECOIL_RIGHT of int

  datatype player_attacked = NOT_ATTACKED | ATTACKED of int

  datatype main_attack =
    MAIN_NOT_ATTACKING
  | MAIN_ATTACKING of {length: int, growing: bool}
  | MAIN_THROWING

  type defeated_enemies = {angle: int}

  type player_projectile = {x: int, y: int, facing: EntityType.facing}

  type player =
    { yAxis: EntityType.y_axis
    , xAxis: EntityType.x_axis
    , recoil: player_recoil
    , attacked: player_attacked
    , mainAttack: main_attack
    , mainAttackPressed: bool
    , facing: EntityType.facing
    , health: int
    , x: int
    , y: int
    , jumpPressed: bool
    , enemies: defeated_enemies vector
    , charge: int
    , projectiles: player_projectile vector
    , platID: int
    }
end
