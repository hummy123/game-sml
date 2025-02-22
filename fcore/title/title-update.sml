structure TitleUpdate =
struct
  open TitleType

  fun update (titleState, input: FrameInputType.t, userKeys, time) =
    case #focus titleState of
      START_BUTTON =>
        let
          val mode =
            if CoreKey.containsAttack (userKeys, #newKeys input) then
              GameType.LEVEL LevelType.initial
            else
              let
                val titleState =
                  if #downHeld input then {focus = OPTIONS_BUTTON}
                  else titleState
              in
                GameType.TITLE titleState
              end
        in
          {mode = mode, userKeys = userKeys, saveKeys = false}
        end
    | OPTIONS_BUTTON =>
        let
          val mode =
            if CoreKey.containsAttack (userKeys, #newKeys input) then
              let val options = OptionsType.init userKeys
              in GameType.OPTIONS options
              end
            else
              let
                val titleState =
                  if #upHeld input then {focus = START_BUTTON} else titleState
              in
                GameType.TITLE titleState
              end
        in
          {mode = mode, userKeys = userKeys, saveKeys = false}
        end
end
