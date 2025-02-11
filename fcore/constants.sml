structure Constants =
struct
  val worldWidth = 1920
  val worldHeight = 1080
  val worldWidthReal: Real32.real = 1920.0
  val worldHeightReal: Real32.real = 1080.0

  (* constants for player *)
  val playerSize = 35
  val playerSizeReal: Real32.real = 35.0
  val halfPlayerSize = 35 div 2
  val halfPlayerSizeReal: Real32.real = 35.0 / 2.0
  val movePlayerBy = 5

  (* player timing values *)
  val jumpLimit = 150
  val floatLimit = 3
  val recoilLimit = 15
  val attackedLimit = 55
  val maxCharge = 60
  val attackLengthLimit = 59

  (* constants for projectiles *)
  val projectilePi: Real32.real = Real32.Math.pi / 180.0
  val projectileSize: Real32.real = 9.0
  val projectileDistance: Real32.real = 13.0
  val projectileSizeInt = 9

  (* constants for enemy *)
  val enemySize = 35
  val enemySizeReal: Real32.real = 35.0
  val moveEnemyBy = 3
  val batRestLimit = 75

  val moveProjectileBy = 11
end
