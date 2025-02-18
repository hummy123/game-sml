signature GAME_TYPE =
sig
  datatype mode =
    LEVEL of LevelType.level_type
end

structure GameType :> GAME_TYPE =
struct
  datatype mode =
    LEVEL of LevelType.level_type

  type game_type = {
    userKeys: CoreKey.user_key,
    mode: mode
  }
end
