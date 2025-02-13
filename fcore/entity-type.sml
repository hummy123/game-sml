signature ENTITY_TYPE =
sig
  datatype y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype facing = FACING_LEFT | FACING_RIGHT
end

structure EntityType :> ENTITY_TYPE =
struct
  datatype y_axis =
    ON_GROUND
  | FALLING
  | DROP_BELOW_PLATFORM
  | JUMPING of int
  | FLOATING of int

  datatype x_axis = MOVE_LEFT | STAY_STILL | MOVE_RIGHT

  datatype facing = FACING_LEFT | FACING_RIGHT
end
