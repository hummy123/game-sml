structure GameUpdate =
struct
  open GameType

  fun update (game: GameType.game_type, input) =
    let
      val {mode, userKeys} = game

      val mode =
        case mode of
          LEVEL level =>
            let val level = LevelUpdate.update (level, input)
            in LEVEL level
            end
        | TITLE title =>
            let val title = TitleUpdate.update (title, input)
            in TITLE title
            end
    in
      {mode = mode, userKeys = userKeys}
    end
end
