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

  type options_type = {focus: focus}

  val initial: options_type
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

  type options_type = {focus: focus}

  val initial = {focus = LEFT_KEY}
end
