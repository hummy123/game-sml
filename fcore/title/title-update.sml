structure TitleUpdate =
struct
  open TitleType

  fun update (titleState, input: FrameInputType.t, userKeys) =
    case #focus titleState of
      START_BUTTON =>
        let
          val mode =
            if #attackHeld input orelse #jumpHeld input then
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
          {mode = mode, userKeys = userKeys}
        end
    | OPTIONS_BUTTON =>
        let
          val mode =
            if #attackHeld input orelse #jumpHeld input then
              (* placeholder: go to configure screen instead once that is implemented *)
              GameType.OPTIONS OptionsType.initial
            else
              let
                val titleState =
                  if #upHeld input then {focus = START_BUTTON} else titleState
              in
                GameType.TITLE titleState
              end
        in
          {mode = mode, userKeys = userKeys}
        end
end
