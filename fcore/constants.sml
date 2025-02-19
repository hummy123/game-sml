structure Constants =
struct
  val fontSpace = 26
  val fontSize: Real32.real = 60.0

  val worldWidth = 1920
  val worldHeight = 1080
  val worldWidthReal: Real32.real = 1920.0
  val worldHeightReal: Real32.real = 1080.0

  (* constants for player *)
  val playerWidth = 32
  val playerHeight = 40
  val playerWidthReal: Real32.real = 32.0
  val playerHeightReal: Real32.real = 40.0

  val halfPlayerWidthReal: Real32.real = 16.0
  val halfPlayerHeightReal: Real32.real = 20.0
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
  val enemySize = 32
  val enemySizeReal: Real32.real = 32.0
  val moveEnemyBy = 3

  val batRestLimit = 55
  val moveBatX = 1
  val moveBatY = 2

  val moveProjectileBy = 11
end
