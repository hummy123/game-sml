signature ENEMY_VARIANTS =
sig
  datatype t = PATROL_SLIME | FOLLOW_SLIME
end

structure EnemyVariants: ENEMY_VARIANTS =
struct datatype t = PATROL_SLIME | FOLLOW_SLIME end
