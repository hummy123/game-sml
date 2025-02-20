structure GameUpdate =
struct
  open GameType

  fun update (game: GameType.game_type, input) =
    let
      val {mode, userKeys} = game
    in
      case mode of
        LEVEL level => LevelUpdate.update (level, input, userKeys)
      | TITLE title => TitleUpdate.update (title, input, userKeys)
      | OPTIONS options => OptionsUpdate.update (options, input, userKeys)
    end
end
