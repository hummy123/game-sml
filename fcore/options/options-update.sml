structure OptionsUpdate =
struct
  open OptionsType

  fun default (options: OptionsType.options_type, userKeys) =
    let
      val {focus, ...} = options
      (* `default` function is called when no keys are pressed
       * so set up pressed/down pressed both to 0
       * as neither is being pressed. *)
      val options = {focus = focus, lastUpPress = 0.0, lastDownPress = 0.0}
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys}
    end

  fun moveFocusUp (options, newFocus, userKeys, time) =
    let
      val {focus, lastUpPress, ...} = options
      (* only switch to newFocus if it is time for key delay to be triggered.
       * We set lastDownPress to 0 because up is currently being pressed instead
       * so we don't want to a key delay for down. *)
      val options =
        if lastUpPress + Constants.keyDelay <= time then
          {focus = newFocus, lastUpPress = time, lastDownPress = 0.0}
        else
          {focus = focus, lastUpPress = lastUpPress, lastDownPress = 0.0}
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys}
    end

  fun moveFocusDown (options, newFocus, userKeys, time) =
    let
      val {focus, lastDownPress, ...} = options
      val options =
        if lastDownPress + Constants.keyDelay <= time then
          {focus = newFocus, lastUpPress = 0.0, lastDownPress = time}
        else
          {focus = focus, lastUpPress = 0.0, lastDownPress = lastDownPress}
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys}
    end

  fun update (options, input: FrameInputType.t, userKeys, time) =
    case #focus options of
      LEFT_KEY =>
        if #downHeld input then
          moveFocusDown (options, RIGHT_KEY, userKeys, time)
        else
          default (options, userKeys)
    | RIGHT_KEY =>
        if #upHeld input then
          moveFocusUp (options, LEFT_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, UP_KEY, userKeys, time)
        else
          default (options, userKeys)
    | UP_KEY =>
        if #upHeld input then
          moveFocusUp (options, RIGHT_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, DOWN_KEY, userKeys, time)
        else
          default (options, userKeys)
    | DOWN_KEY =>
        if #upHeld input then
          moveFocusUp (options, UP_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, JUMP_KEY, userKeys, time)
        else
          default (options, userKeys)
    | JUMP_KEY =>
        if #upHeld input then
          moveFocusUp (options, DOWN_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, ATTACK_KEY, userKeys, time)
        else
          default (options, userKeys)
    | ATTACK_KEY =>
        if #upHeld input then
          moveFocusUp (options, JUMP_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, SAVE_BUTTON, userKeys, time)
        else
          default (options, userKeys)
    | SAVE_BUTTON =>
        if #upHeld input then
          moveFocusUp (options, ATTACK_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, CANCEL_BUTTON, userKeys, time)
        else
          default (options, userKeys)
    | CANCEL_BUTTON =>
        if #upHeld input then moveFocusUp (options, SAVE_BUTTON, userKeys, time)
        else default (options, userKeys)
end
