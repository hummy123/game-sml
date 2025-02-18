structure FallingEnemyPair =
struct
  type key = int
  type value = EnemyType.falling_enemy

  fun l (a: int, b: int) = a < b
  fun eq (a: int, b: int) = a = b
  fun g (a: int, b: int) = a > b

  val maxNodeSize = 8
end
