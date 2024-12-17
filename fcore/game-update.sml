structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree, platforms, platformTree} = game
      val player = Player.move (game, input)
    in
      { player = player
      , walls = walls
      , wallTree = wallTree
      , platforms = platforms
      , platformTree = platformTree
      }
    end
end
