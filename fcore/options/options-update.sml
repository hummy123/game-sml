signature MAKE_UPDATE_SELECTED_KEY =
sig
  val updateKeys: CoreKey.key_code * CoreKey.user_key -> CoreKey.user_key
  val default: OptionsType.options_type * CoreKey.user_key -> GameType.game_type
  val deselect: OptionsType.options_type * CoreKey.user_key
                -> GameType.game_type
end

functor MakeUpdateSelectedKey(Fn: MAKE_UPDATE_SELECTED_KEY) =
struct
  fun setNewKeys (options, tempKeys, userKeys, time) =
    let
      val {focus, lastUpPress, lastDownPress, ...} = options
      val options =
        { focus = focus
        , lastUpPress = time
        , lastDownPress = time
        , isSelected = false
        , tempKeys = tempKeys
        }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun onSelected (options, input: FrameInputType.t, userKeys, time) =
    case #newKeys input of
      key :: tl =>
        (* change key *)
        if key = CoreKey.KEY_ESCAPE orelse CoreKey.containsEscape (userKeys, tl) then
          (* deslect as that is the function of the escape key *)
          Fn.deselect (options, userKeys)
        (* what if new key collides with existing key? todo *)
        else
          let val tempKeys = Fn.updateKeys (key, #tempKeys options)
          in setNewKeys (options, tempKeys, userKeys, time)
          end
    | [] => Fn.default (options, userKeys)
end

structure OptionsUpdate =
struct
  open OptionsType

  fun default (options: OptionsType.options_type, userKeys) =
    let
      val {focus, isSelected, tempKeys, ...} = options
      (* `default` function is called when no keys are pressed
       * so set up pressed/down pressed both to 0
       * as neither is being pressed. *)
      val options =
        { focus = focus
        , lastUpPress = 0.0
        , lastDownPress = 0.0
        , isSelected = isSelected
        , tempKeys = tempKeys
        }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun moveFocusUp (options: OptionsType.options_type, newFocus, userKeys, time) =
    let
      val {focus, isSelected, lastUpPress, tempKeys, ...} = options
      (* only switch to newFocus if it is time for key delay to be triggered.
       * We set lastDownPress to 0 because up is currently being pressed instead
       * so we don't want to a key delay for down. *)
      val options =
        if lastUpPress + Constants.keyDelay <= time then
          { focus = newFocus
          , lastUpPress = time
          , lastDownPress = 0.0
          , isSelected = isSelected
          , tempKeys = tempKeys
          }
        else
          { focus = focus
          , lastUpPress = lastUpPress
          , lastDownPress = 0.0
          , isSelected = isSelected
          , tempKeys = tempKeys
          }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun moveFocusDown
    (options: OptionsType.options_type, newFocus, userKeys, time) =
    let
      val {focus, isSelected, lastDownPress, tempKeys, ...} = options
      val options =
        if lastDownPress + Constants.keyDelay <= time then
          { focus = newFocus
          , lastUpPress = 0.0
          , lastDownPress = time
          , isSelected = isSelected
          , tempKeys = tempKeys
          }
        else
          { focus = focus
          , lastUpPress = 0.0
          , lastDownPress = lastDownPress
          , isSelected = isSelected
          , tempKeys = tempKeys
          }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun select (options: OptionsType.options_type, userKeys) =
    let
      val {focus, lastUpPress, lastDownPress, tempKeys, ...} = options
      val options =
        { focus = focus
        , lastUpPress = lastUpPress
        , lastDownPress = lastDownPress
        , isSelected = true
        , tempKeys = tempKeys
        }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun deselect (options: OptionsType.options_type, userKeys) =
    let
      val {focus, lastUpPress, lastDownPress, tempKeys, ...} = options
      val options =
        { focus = focus
        , lastUpPress = lastUpPress
        , lastDownPress = lastDownPress
        , isSelected = false
        , tempKeys = tempKeys
        }
    in
      {mode = GameType.OPTIONS options, userKeys = userKeys, saveKeys = false}
    end

  fun withLeftKeys (newLeft, userKeys: CoreKey.user_key) =
    let
      val {right, up, down, jump, attack, escape, ...} = userKeys
    in
      { left = newLeft
      , right = right
      , up = up
      , down = down
      , jump = jump
      , attack = attack
      , escape = escape
      }
    end

  fun withRightKeys (newRight, userKeys: CoreKey.user_key) =
    let
      val {left, up, down, jump, attack, escape, ...} = userKeys
    in
      { left = left
      , right = newRight
      , up = up
      , down = down
      , jump = jump
      , attack = attack
      , escape = escape
      }
    end

  fun withUpKeys (newUp, userKeys: CoreKey.user_key) =
    let
      val {left, right, down, jump, attack, escape, ...} = userKeys
    in
      { left = left
      , right = right
      , up = newUp
      , down = down
      , jump = jump
      , attack = attack
      , escape = escape
      }
    end

  fun withDownKeys (newDown, userKeys: CoreKey.user_key) =
    let
      val {left, right, up, jump, attack, escape, ...} = userKeys
    in
      { left = left
      , right = right
      , up = up
      , down = newDown
      , jump = jump
      , attack = attack
      , escape = escape
      }
    end

  fun withJumpKeys (newJump, userKeys: CoreKey.user_key) =
    let
      val {left, right, up, down, attack, escape, ...} = userKeys
    in
      { left = left
      , right = right
      , up = up
      , down = down
      , jump = newJump
      , attack = attack
      , escape = escape
      }
    end

  fun withAttackKeys (newAttack, userKeys: CoreKey.user_key) =
    let
      val {left, right, up, down, jump, escape, ...} = userKeys
    in
      { left = left
      , right = right
      , up = up
      , down = down
      , jump = jump
      , attack = newAttack
      , escape = escape
      }
    end

  structure UpdateLeftKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withLeftKeys
         val default = default
         val deselect = deselect
       end)

  structure UpdateRightKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withRightKeys
         val default = default
         val deselect = deselect
       end)

  structure UpdateUpKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withUpKeys
         val default = default
         val deselect = deselect
       end)

  structure UpdateDownKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withDownKeys
         val default = default
         val deselect = deselect
       end)

  structure UpdateJumpKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withJumpKeys
         val default = default
         val deselect = deselect
       end)

  structure UpdateAttackKey =
    MakeUpdateSelectedKey
      (struct
         val updateKeys = withAttackKeys
         val default = default
         val deselect = deselect
       end)

  fun saveAndGoToTitle options =
    let
      val userKeys = #tempKeys options
    in
      { mode = GameType.TITLE TitleType.initial
      , userKeys = userKeys
      , saveKeys = true
      }
    end

  fun cancelAndGoToTitle userKeys =
    { mode = GameType.TITLE TitleType.initial
    , userKeys = userKeys
    , saveKeys = false
    }

  fun update (options, input: FrameInputType.t, userKeys, time) =
    case #focus options of
      LEFT_KEY =>
        if #isSelected options then
          UpdateLeftKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, CANCEL_BUTTON, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, RIGHT_KEY, userKeys, time)
        else
          default (options, userKeys)
    | RIGHT_KEY =>
        if #isSelected options then
          UpdateRightKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, LEFT_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, UP_KEY, userKeys, time)
        else
          default (options, userKeys)
    | UP_KEY =>
        if #isSelected options then
          UpdateUpKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, RIGHT_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, DOWN_KEY, userKeys, time)
        else
          default (options, userKeys)
    | DOWN_KEY =>
        if #isSelected options then
          UpdateDownKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, UP_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, JUMP_KEY, userKeys, time)
        else
          default (options, userKeys)
    | JUMP_KEY =>
        if #isSelected options then
          UpdateJumpKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, DOWN_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, ATTACK_KEY, userKeys, time)
        else
          default (options, userKeys)
    | ATTACK_KEY =>
        if #isSelected options then
          UpdateAttackKey.onSelected (options, input, userKeys, time)
        else if CoreKey.containsAttack (userKeys, #newKeys input) then
          select (options, userKeys)
        else if #upHeld input then
          moveFocusUp (options, JUMP_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, SAVE_BUTTON, userKeys, time)
        else
          default (options, userKeys)
    | SAVE_BUTTON =>
        if CoreKey.containsAttack (userKeys, #newKeys input) then
          saveAndGoToTitle options
        else if #upHeld input then
          moveFocusUp (options, ATTACK_KEY, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, CANCEL_BUTTON, userKeys, time)
        else
          default (options, userKeys)
    | CANCEL_BUTTON =>
        if CoreKey.containsAttack (userKeys, #newKeys input) then
          cancelAndGoToTitle userKeys
        else if #upHeld input then
          moveFocusUp (options, SAVE_BUTTON, userKeys, time)
        else if #downHeld input then
          moveFocusDown (options, LEFT_KEY, userKeys, time)
        else
          default (options, userKeys)
end
