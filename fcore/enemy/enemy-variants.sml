signature ENEMY_VARIANTS =
sig
  datatype t = PATROL_SLIME | FOLLOW_SLIME | STRAIGHT_BAT
end

structure EnemyVariants: ENEMY_VARIANTS =
struct datatype t = PATROL_SLIME | FOLLOW_SLIME | STRAIGHT_BAT end
