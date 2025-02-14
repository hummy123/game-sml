structure Projectile =
struct
  val projectileSize = 9.0
  val projectileSizeInt = 9

  fun helpGenerateTree (pos, projectiles, acc) =
    if pos = Vector.length projectiles then
      acc
    else
      let
        val size = projectileSizeInt

        val {x, y, facing = _} = Vector.sub (projectiles, pos)
        val acc = QuadTree.insert (x, y, size, size, pos, acc)
      in
        helpGenerateTree (pos + 1, projectiles, acc)
      end

  fun generateTree projectiles =
    helpGenerateTree
      ( 0
      , projectiles
      , QuadTree.create (Constants.worldWidth, Constants.worldHeight)
      )

  fun helpGetProjectileVec
    (pos, projectiles, width, height, ratio, xOffset, yOffset, acc) =
    if pos = Vector.length projectiles then
      Vector.concat acc
    else
      let
        val {x, y, ...} = Vector.sub (projectiles, pos)

        val x = Real32.fromInt x * ratio + xOffset
        val y = Real32.fromInt y * ratio + yOffset

        val size = projectileSize * ratio

        val vec = Field.lerp
          (x, y, size, size, width, height, 0.3, 0.9, 0.3, 1.0)
        val acc = vec :: acc
      in
        helpGetProjectileVec
          (pos + 1, projectiles, width, height, ratio, xOffset, yOffset, acc)
      end

  fun getProjectileVec (player: PlayerType.player, width, height) =
    let
      val {projectiles, ...} = player

      val wratio = width / 1920.0
      val hratio = height / 1080.0
    in
      if wratio < hratio then
        let
          val scale = 1080.0 * wratio
          val yOffset =
            if height > scale then (height - scale) / 2.0
            else if height < scale then (scale - height) / 2.0
            else 0.0

          val xOffset = 0.0
        in
          helpGetProjectileVec
            (0, projectiles, width, height, wratio, xOffset, yOffset, [])
        end
      else
        let
          val scale = 1920.0 * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0

          val yOffset = 0.0
        in
          helpGetProjectileVec
            (0, projectiles, width, height, hratio, xOffset, yOffset, [])
        end
    end
end
