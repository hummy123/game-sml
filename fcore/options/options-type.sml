signature OPTIONS_TYPE =
sig
  datatype focus =
    LEFT_KEY
  | RIGHT_KEY
  | UP_KEY
  | DOWN_KEY
  | JUMP_KEY
  | ATTACK_KEY
  | SAVE_BUTTON
  | CANCEL_BUTTON

  type options_type =
    { focus: focus
    , isSelected: bool
    , lastUpPress: real
    , lastDownPress: real
    , tempKeys: CoreKey.user_key
    }

  val init: CoreKey.user_key -> options_type
end

structure OptionsType :> OPTIONS_TYPE =
struct
  datatype focus =
    LEFT_KEY
  | RIGHT_KEY
  | UP_KEY
  | DOWN_KEY
  | JUMP_KEY
  | ATTACK_KEY
  | SAVE_BUTTON
  | CANCEL_BUTTON

  type options_type =
    { focus: focus
    , isSelected: bool
    , lastUpPress: real
    , lastDownPress: real
    , tempKeys: CoreKey.user_key
    }

  fun init userKeys =
    { focus = LEFT_KEY
    , isSelected = false
    , lastUpPress = 0.0
    , lastDownPress = 0.0
    , tempKeys = userKeys
    }
end
