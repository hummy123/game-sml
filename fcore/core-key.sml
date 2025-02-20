signature CORE_KEY =
sig
  (* enumeration of keys, following GLFW names
   * taken from https://www.glfw.org/docs/latest/group__keys.html
   * note that there is no dependency on GLFW here: it is just a plain datatype.
   * *)
  datatype key_code =
    KEY_SPACE
  | KEY_APOSTROPHE
  | KEY_COMMA
  | KEY_MINUS
  | KEY_PERIOD
  | KEY_SLASH
  | KEY_0
  | KEY_1
  | KEY_2
  | KEY_3
  | KEY_4
  | KEY_5
  | KEY_6
  | KEY_7
  | KEY_8
  | KEY_9
  | KEY_SEMICOLON
  | KEY_EQUAL
  | KEY_A
  | KEY_B
  | KEY_C
  | KEY_D
  | KEY_E
  | KEY_F
  | KEY_G
  | KEY_H
  | KEY_I
  | KEY_J
  | KEY_K
  | KEY_L
  | KEY_M
  | KEY_N
  | KEY_O
  | KEY_P
  | KEY_Q
  | KEY_R
  | KEY_S
  | KEY_T
  | KEY_U
  | KEY_V
  | KEY_W
  | KEY_X
  | KEY_Y
  | KEY_Z
  | KEY_LEFT_BRACKET
  | KEY_BACKSLASH
  | KEY_RIGHT_BRACKET
  | KEY_GRAVE_ACCENT
  | KEY_WORLD_1
  | KEY_WORLD_2
  | KEY_ESCAPE
  | KEY_ENTER
  | KEY_TAB
  | KEY_BACKSPACE
  | KEY_INSERT
  | KEY_DELETE
  | KEY_LEFT
  | KEY_RIGHT
  | KEY_DOWN
  | KEY_UP
  | KEY_PAGE_UP
  | KEY_PAGE_DOWN
  | KEY_HOME
  | KEY_END
  | KEY_CAPS_LOCK
  | KEY_SCROLL_LOCK
  | KEY_NUM_LOCK
  | KEY_PRINT_SCREEN
  | KEY_PAUSE
  | KEY_F1
  | KEY_F2
  | KEY_F3
  | KEY_F4
  | KEY_F5
  | KEY_F6
  | KEY_F7
  | KEY_F8
  | KEY_F9
  | KEY_F10
  | KEY_F11
  | KEY_F12
  | KEY_F13
  | KEY_F14
  | KEY_F15
  | KEY_F16
  | KEY_F17
  | KEY_F18
  | KEY_F19
  | KEY_F20
  | KEY_F21
  | KEY_F22
  | KEY_F23
  | KEY_F24
  | KEY_F25
  | KEY_KP_0
  | KEY_KP_1
  | KEY_KP_2
  | KEY_KP_3
  | KEY_KP_4
  | KEY_KP_5
  | KEY_KP_6
  | KEY_KP_7
  | KEY_KP_8
  | KEY_KP_9
  | KEY_KP_DECIMAL
  | KEY_KP_DIVIDE
  | KEY_KP_MULTIPLY
  | KEY_KP_SUBTRACT
  | KEY_KP_ADD
  | KEY_KP_ENTER
  | KEY_KP_EQUAL
  | KEY_LEFT_SHIFT
  | KEY_LEFT_CONTROL
  | KEY_LEFT_ALT
  | KEY_LEFT_SUPER
  | KEY_RIGHT_SHIFT
  | KEY_RIGHT_CONTROL
  | KEY_RIGHT_ALT
  | KEY_RIGHT_SUPER
  | KEY_MENU

  (* user's chosen key mappings *)
  type user_key =
    { left: key_code
    , right: key_code
    , up: key_code
    , down: key_code
    , jump: key_code
    , attack: key_code
    , escape: key_code
    }

  val keyFromString: string -> key_code option
end

structure CoreKey :> CORE_KEY =
struct
  (* enumeration of keys, following GLFW names
   * taken from https://www.glfw.org/docs/latest/group__keys.html
   * note that there is no dependency on GLFW here: it is just a plain datatype.
   * *)
  datatype key_code =
    KEY_SPACE
  | KEY_APOSTROPHE
  | KEY_COMMA
  | KEY_MINUS
  | KEY_PERIOD
  | KEY_SLASH
  | KEY_0
  | KEY_1
  | KEY_2
  | KEY_3
  | KEY_4
  | KEY_5
  | KEY_6
  | KEY_7
  | KEY_8
  | KEY_9
  | KEY_SEMICOLON
  | KEY_EQUAL
  | KEY_A
  | KEY_B
  | KEY_C
  | KEY_D
  | KEY_E
  | KEY_F
  | KEY_G
  | KEY_H
  | KEY_I
  | KEY_J
  | KEY_K
  | KEY_L
  | KEY_M
  | KEY_N
  | KEY_O
  | KEY_P
  | KEY_Q
  | KEY_R
  | KEY_S
  | KEY_T
  | KEY_U
  | KEY_V
  | KEY_W
  | KEY_X
  | KEY_Y
  | KEY_Z
  | KEY_LEFT_BRACKET
  | KEY_BACKSLASH
  | KEY_RIGHT_BRACKET
  | KEY_GRAVE_ACCENT
  | KEY_WORLD_1
  | KEY_WORLD_2
  | KEY_ESCAPE
  | KEY_ENTER
  | KEY_TAB
  | KEY_BACKSPACE
  | KEY_INSERT
  | KEY_DELETE
  | KEY_LEFT
  | KEY_RIGHT
  | KEY_DOWN
  | KEY_UP
  | KEY_PAGE_UP
  | KEY_PAGE_DOWN
  | KEY_HOME
  | KEY_END
  | KEY_CAPS_LOCK
  | KEY_SCROLL_LOCK
  | KEY_NUM_LOCK
  | KEY_PRINT_SCREEN
  | KEY_PAUSE
  | KEY_F1
  | KEY_F2
  | KEY_F3
  | KEY_F4
  | KEY_F5
  | KEY_F6
  | KEY_F7
  | KEY_F8
  | KEY_F9
  | KEY_F10
  | KEY_F11
  | KEY_F12
  | KEY_F13
  | KEY_F14
  | KEY_F15
  | KEY_F16
  | KEY_F17
  | KEY_F18
  | KEY_F19
  | KEY_F20
  | KEY_F21
  | KEY_F22
  | KEY_F23
  | KEY_F24
  | KEY_F25
  | KEY_KP_0
  | KEY_KP_1
  | KEY_KP_2
  | KEY_KP_3
  | KEY_KP_4
  | KEY_KP_5
  | KEY_KP_6
  | KEY_KP_7
  | KEY_KP_8
  | KEY_KP_9
  | KEY_KP_DECIMAL
  | KEY_KP_DIVIDE
  | KEY_KP_MULTIPLY
  | KEY_KP_SUBTRACT
  | KEY_KP_ADD
  | KEY_KP_ENTER
  | KEY_KP_EQUAL
  | KEY_LEFT_SHIFT
  | KEY_LEFT_CONTROL
  | KEY_LEFT_ALT
  | KEY_LEFT_SUPER
  | KEY_RIGHT_SHIFT
  | KEY_RIGHT_CONTROL
  | KEY_RIGHT_ALT
  | KEY_RIGHT_SUPER
  | KEY_MENU

  (* user's chosen key mappings *)
  type user_key =
    { left: key_code
    , right: key_code
    , up: key_code
    , down: key_code
    , jump: key_code
    , attack: key_code
    , escape: key_code
    }

  fun keyFromString str =
    case str of
      "KEY_SPACE" => SOME KEY_SPACE
    | "KEY_APOSTROPHE" => SOME KEY_APOSTROPHE
    | "KEY_COMMA" => SOME KEY_COMMA
    | "KEY_MINUS" => SOME KEY_MINUS
    | "KEY_PERIOD" => SOME KEY_PERIOD
    | "KEY_SLASH" => SOME KEY_SLASH
    | "KEY_0" => SOME KEY_0
    | "KEY_1" => SOME KEY_1
    | "KEY_2" => SOME KEY_2
    | "KEY_3" => SOME KEY_3
    | "KEY_4" => SOME KEY_4
    | "KEY_5" => SOME KEY_5
    | "KEY_6" => SOME KEY_6
    | "KEY_7" => SOME KEY_7
    | "KEY_8" => SOME KEY_8
    | "KEY_9" => SOME KEY_9
    | "KEY_SEMICOLON" => SOME KEY_SEMICOLON
    | "KEY_EQUAL" => SOME KEY_EQUAL
    | "KEY_A" => SOME KEY_A
    | "KEY_B" => SOME KEY_B
    | "KEY_C" => SOME KEY_C
    | "KEY_D" => SOME KEY_D
    | "KEY_E" => SOME KEY_E
    | "KEY_F" => SOME KEY_F
    | "KEY_G" => SOME KEY_G
    | "KEY_H" => SOME KEY_H
    | "KEY_I" => SOME KEY_I
    | "KEY_J" => SOME KEY_J
    | "KEY_K" => SOME KEY_K
    | "KEY_L" => SOME KEY_L
    | "KEY_M" => SOME KEY_M
    | "KEY_N" => SOME KEY_N
    | "KEY_O" => SOME KEY_O
    | "KEY_P" => SOME KEY_P
    | "KEY_Q" => SOME KEY_Q
    | "KEY_R" => SOME KEY_R
    | "KEY_S" => SOME KEY_S
    | "KEY_T" => SOME KEY_T
    | "KEY_U" => SOME KEY_U
    | "KEY_V" => SOME KEY_V
    | "KEY_W" => SOME KEY_W
    | "KEY_X" => SOME KEY_X
    | "KEY_Y" => SOME KEY_Y
    | "KEY_Z" => SOME KEY_Z
    | "KEY_LEFT_BRACKET" => SOME KEY_LEFT_BRACKET
    | "KEY_BACKSLASH" => SOME KEY_BACKSLASH
    | "KEY_RIGHT_BRACKET" => SOME KEY_RIGHT_BRACKET
    | "KEY_GRAVE_ACCENT" => SOME KEY_GRAVE_ACCENT
    | "KEY_WORLD_1" => SOME KEY_WORLD_1
    | "KEY_WORLD_2" => SOME KEY_WORLD_2
    | "KEY_ESCAPE" => SOME KEY_ESCAPE
    | "KEY_ENTER" => SOME KEY_ENTER
    | "KEY_TAB" => SOME KEY_TAB
    | "KEY_BACKSPACE" => SOME KEY_BACKSPACE
    | "KEY_INSERT" => SOME KEY_INSERT
    | "KEY_DELETE" => SOME KEY_DELETE
    | "KEY_LEFT" => SOME KEY_LEFT
    | "KEY_RIGHT" => SOME KEY_RIGHT
    | "KEY_DOWN" => SOME KEY_DOWN
    | "KEY_UP" => SOME KEY_UP
    | "KEY_PAGE_UP" => SOME KEY_PAGE_UP
    | "KEY_PAGE_DOWN" => SOME KEY_PAGE_DOWN
    | "KEY_HOME" => SOME KEY_HOME
    | "KEY_END" => SOME KEY_END
    | "KEY_CAPS_LOCK" => SOME KEY_CAPS_LOCK
    | "KEY_SCROLL_LOCK" => SOME KEY_SCROLL_LOCK
    | "KEY_NUM_LOCK" => SOME KEY_NUM_LOCK
    | "KEY_PRINT_SCREEN" => SOME KEY_PRINT_SCREEN
    | "KEY_PAUSE" => SOME KEY_PAUSE
    | "KEY_F1" => SOME KEY_F1
    | "KEY_F2" => SOME KEY_F2
    | "KEY_F3" => SOME KEY_F3
    | "KEY_F4" => SOME KEY_F4
    | "KEY_F5" => SOME KEY_F5
    | "KEY_F6" => SOME KEY_F6
    | "KEY_F7" => SOME KEY_F7
    | "KEY_F8" => SOME KEY_F8
    | "KEY_F9" => SOME KEY_F9
    | "KEY_F10" => SOME KEY_F10
    | "KEY_F11" => SOME KEY_F11
    | "KEY_F12" => SOME KEY_F12
    | "KEY_F13" => SOME KEY_F13
    | "KEY_F14" => SOME KEY_F14
    | "KEY_F15" => SOME KEY_F15
    | "KEY_F16" => SOME KEY_F16
    | "KEY_F17" => SOME KEY_F17
    | "KEY_F18" => SOME KEY_F18
    | "KEY_F19" => SOME KEY_F19
    | "KEY_F20" => SOME KEY_F20
    | "KEY_F21" => SOME KEY_F21
    | "KEY_F22" => SOME KEY_F22
    | "KEY_F23" => SOME KEY_F23
    | "KEY_F24" => SOME KEY_F24
    | "KEY_F25" => SOME KEY_F25
    | "KEY_KP_0" => SOME KEY_KP_0
    | "KEY_KP_1" => SOME KEY_KP_1
    | "KEY_KP_2" => SOME KEY_KP_2
    | "KEY_KP_3" => SOME KEY_KP_3
    | "KEY_KP_4" => SOME KEY_KP_4
    | "KEY_KP_5" => SOME KEY_KP_5
    | "KEY_KP_6" => SOME KEY_KP_6
    | "KEY_KP_7" => SOME KEY_KP_7
    | "KEY_KP_8" => SOME KEY_KP_8
    | "KEY_KP_9" => SOME KEY_KP_9
    | "KEY_KP_DECIMAL" => SOME KEY_KP_DECIMAL
    | "KEY_KP_DIVIDE" => SOME KEY_KP_DIVIDE
    | "KEY_KP_MULTIPLY" => SOME KEY_KP_MULTIPLY
    | "KEY_KP_SUBTRACT" => SOME KEY_KP_SUBTRACT
    | "KEY_KP_ADD" => SOME KEY_KP_ADD
    | "KEY_KP_ENTER" => SOME KEY_KP_ENTER
    | "KEY_KP_EQUAL" => SOME KEY_KP_EQUAL
    | "KEY_LEFT_SHIFT" => SOME KEY_LEFT_SHIFT
    | "KEY_LEFT_CONTROL" => SOME KEY_LEFT_CONTROL
    | "KEY_LEFT_ALT" => SOME KEY_LEFT_ALT
    | "KEY_LEFT_SUPER" => SOME KEY_LEFT_SUPER
    | "KEY_RIGHT_SHIFT" => SOME KEY_RIGHT_SHIFT
    | "KEY_RIGHT_CONTROL" => SOME KEY_RIGHT_CONTROL
    | "KEY_RIGHT_ALT" => SOME KEY_RIGHT_ALT
    | "KEY_RIGHT_SUPER" => SOME KEY_RIGHT_SUPER
    | "KEY_MENU" => SOME KEY_MENU
    | _ => NONE
end
