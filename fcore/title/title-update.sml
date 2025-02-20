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
              GameType.TITLE titleState
        in
          {mode = mode, userKeys = userKeys}
        end
end
