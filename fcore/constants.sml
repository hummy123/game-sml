structure Constants =
struct
  val worldWidth = 1920
  val worldHeight = 1080

  (* constants for player *)
  val playerSize = 35
  val playerSizeReal = 35.0
  val halfPlayerSize = 35 div 2
  val halfPlayerSizeReal = 35.0 / 2.0
  val movePlayerBy = 5

  (* player timing values *)
  val jumpLimit = 150
  val floatLimit = 3
  val recoilLimit = 15
  val attackedLimit = 55
  val maxCharge = 60

  (* constants for projectiles *)
  val projectilePi: Real32.real = Real32.Math.pi / 180.0
  val projectileSize: Real32.real = 9.0
  val projectileDistance: Real32.real = 13.0
  val projectileSizeInt = 9
end
