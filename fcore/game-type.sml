signature GAME_TYPE =
sig
  datatype mode = LEVEL of LevelType.level_type

  type game_type = {userKeys: CoreKey.user_key, mode: mode}

  val init: CoreKey.user_key -> game_type
end

structure GameType :> GAME_TYPE =
struct
  datatype mode = LEVEL of LevelType.level_type

  type game_type = {userKeys: CoreKey.user_key, mode: mode}

  fun init userKeys =
    let val mode = LEVEL LevelType.initial
    in {mode = mode, userKeys = userKeys}
    end
end
