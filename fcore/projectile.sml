structure Projectile =
struct
  fun helpGenerateTree (pos, projectiles, acc) =
    if pos = Vector.length projectiles then
      acc
    else
      let
        val size = Player.defeatedSizeInt

        val {x, y, facing = _} = Vector.sub (projectiles, pos)
        val acc = QuadTree.insert (x, y, size, size, 0, 0, 1920, 1080, pos, acc)
      in
        helpGenerateTree (pos + 1, projectiles, acc)
      end

  fun generateTree projectiles =
    helpGenerateTree (0, projectiles, QuadTree.empty)
end
