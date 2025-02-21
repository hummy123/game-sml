structure GameUpdate =
struct
  open GameType

  fun update (game: GameType.game_type, input, time) =
    let
      val {mode, userKeys} = game
    in
      case mode of
        LEVEL level => LevelUpdate.update (level, input, userKeys, time)
      | TITLE title => TitleUpdate.update (title, input, userKeys, time)
      | OPTIONS options => OptionsUpdate.update (options, input, userKeys, time)
    end
end
