structure ProjectileEnemy =
struct
  open GameType

  fun helpCheckColisions (pos, projectileTree, enemies, acc) =
    if pos < 0 then
      Vector.fromList acc
    else
      let
        val enemy = Vector.sub (enemies, pos)

        val {id, health, x, y} = enemy
        val size = Enemy.size

        val collisions = QuadTree.helpGetCollisions
          (x, y, size, size, 0, 0, 1920, 1080, 0, [], projectileTree)

        (* react to collisions here, possibly removing enemy from acc *)
        val acc =
          case collisions of
            _ :: _ =>
              if health = 1 then
                (* filter enemy out if decrementing their health by 1 
                 * leads to a health of 0 *)
                acc
              else
                let
                  (* decrement health by 1 and add to acc *)
                  val enemy = {id = id, health = health - 1, x = x, y = y}
                in
                  enemy :: acc
                end
          | [] => enemy :: acc
      in
        helpCheckColisions (pos - 1, projectileTree, enemies, acc)
      end

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

  fun checkCollisions (projectiles, enemies, enemyTree) =
    let
      val projectileTree = generateTree projectiles
    in
      helpCheckColisions
        (Vector.length enemies - 1, projectileTree, enemies, [])
    end
end
