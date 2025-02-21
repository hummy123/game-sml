structure OptionsUpdate =
struct
  open OptionsType

  fun default (options, userKeys) =
    {mode = GameType.OPTIONS options, userKeys = userKeys}

  fun withFocus (newFocus, userKeys) =
    {mode = GameType.OPTIONS {focus = newFocus}, userKeys = userKeys}

  fun update (options, input: FrameInputType.t, userKeys) =
    case #focus options of
      LEFT_KEY =>
        if #downHeld input then withFocus (RIGHT_KEY, userKeys)
        else default (options, userKeys)
    | RIGHT_KEY =>
        if #upHeld input then withFocus (LEFT_KEY, userKeys)
        else if #downHeld input then withFocus (UP_KEY, userKeys)
        else default (options, userKeys)
    | UP_KEY =>
        if #upHeld input then withFocus (RIGHT_KEY, userKeys)
        else if #downHeld input then withFocus (DOWN_KEY, userKeys)
        else default (options, userKeys)
    | DOWN_KEY =>
        if #upHeld input then withFocus (UP_KEY, userKeys)
        else if #downHeld input then withFocus (JUMP_KEY, userKeys)
        else default (options, userKeys)
    | JUMP_KEY =>
        if #upHeld input then withFocus (DOWN_KEY, userKeys)
        else if #downHeld input then withFocus (ATTACK_KEY, userKeys)
        else default (options, userKeys)
    | ATTACK_KEY =>
        if #upHeld input then withFocus (JUMP_KEY, userKeys)
        else if #downHeld input then withFocus (SAVE_BUTTON, userKeys)
        else default (options, userKeys)
    | SAVE_BUTTON =>
        if #upHeld input then withFocus (ATTACK_KEY, userKeys)
        else if #downHeld input then withFocus (CANCEL_BUTTON, userKeys)
        else default (options, userKeys)
    | CANCEL_BUTTON =>
        if #upHeld input then withFocus (SAVE_BUTTON, userKeys)
        else default (options, userKeys)
end
