structure GameUpdate =
struct
  fun update (game, input) =
    let
      val {player, walls, wallTree} = game
      val player = Player.move (game, input)
    in
      {player = player, walls = walls, wallTree = wallTree}
    end
end
