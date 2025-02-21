signature GAME_TYPE =
sig
  datatype mode =
    LEVEL of LevelType.level_type
  | TITLE of TitleType.title_type
  | OPTIONS of OptionsType.options_type

  type game_type = {mode: mode, userKeys: CoreKey.user_key, saveKeys: bool}

  val init: CoreKey.user_key -> game_type
end

structure GameType :> GAME_TYPE =
struct
  datatype mode =
    LEVEL of LevelType.level_type
  | TITLE of TitleType.title_type
  | OPTIONS of OptionsType.options_type

  type game_type = {mode: mode, userKeys: CoreKey.user_key, saveKeys: bool}

  fun init userKeys =
    let val mode = TITLE TitleType.initial
    in {mode = mode, userKeys = userKeys, saveKeys = false}
    end
end
