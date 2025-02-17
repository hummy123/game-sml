structure ParseControls =
struct
  datatype action =
    ACTION_LEFT
  | ACTION_RIGHT
  | ACTION_UP
  | ACTION_DOWN
  | ACTION_JUMP
  | ACTION_ATTACK

  fun actionFromString str =
    case str of
      "ACTION_LEFT" => SOME ACTION_LEFT
    | "ACTION_RIGHT" => SOME ACTION_RIGHT
    | "ACTION_UP" => SOME ACTION_UP
    | "ACTION_DOWN" => SOME ACTION_DOWN
    | "ACTTION_JUMP" => SOME ACTION_JUMP
    | "ACTION_ATTACK" => SOME ACTION_ATTACK
    | _ => NONE

  fun findColon (pos, str) =
    if pos = String.size str then
      ~1
    else
      let val chr = String.sub (str, pos)
      in if chr = #":" then pos else findColon (pos + 1, str)
      end

  type parsed_keys =
    { left: CoreKey.key_code option
    , right: CoreKey.key_code option
    , up: CoreKey.key_code option
    , down: CoreKey.key_code option
    , jump: CoreKey.key_code option
    , attack: CoreKey.key_code option
    }

  fun updateControls (action, key, controls: parsed_keys) =
    let
      val {left, right, up, down, jump, attack} = controls
    in
      case action of
        ACTION_LEFT =>
          { left = SOME key
          , right = right
          , up = up
          , down = down
          , jump = jump
          , attack = attack
          }
      | ACTION_RIGHT =>
          { left = left
          , right = SOME key
          , up = up
          , down = down
          , jump = jump
          , attack = attack
          }
      | ACTION_UP =>
          { left = left
          , right = right
          , up = SOME key
          , down = down
          , jump = jump
          , attack = attack
          }
      | ACTION_DOWN =>
          { left = left
          , right = right
          , up = up
          , down = SOME key
          , jump = jump
          , attack = attack
          }
      | ACTION_JUMP =>
          { left = left
          , right = right
          , up = up
          , down = down
          , jump = SOME key
          , attack = attack
          }
      | ACTION_ATTACK =>
          { left = left
          , right = right
          , up = up
          , down = down
          , jump = jump
          , attack = SOME key
          }
    end

  fun returnControls controls =
    let
      val {left, right, up, down, jump, attack} = controls
    in
      case (left, right, up, down, jump, attack) of
        (SOME left, SOME right, SOME up, SOME down, SOME jump, SOME attack) =>
          SOME
            { left = left
            , right = right
            , up = up
            , down = down
            , jump = jump
            , attack = attack
            }
      | _ => NONE
    end

  fun helpParse (controls, io) =
    case TextIO.inputLine io of
      SOME line =>
        let
          val colon = findColon (0, line)
        in
          if colon = ~1 then
            helpParse (controls, io)
          else
            let
              val actionStart = colon + 1
              val actionLength = String.size line - actionStart
              val actionString =
                String.substring (line, actionStart, actionLength)
              val action = actionFromString actionString

              val keyString = String.substring (line, 0, colon)
              val key = CoreKey.keyFromString keyString

              val controls =
                (case (action, key) of
                   (SOME action, SOME key) =>
                     updateControls (action, key, controls)
                 | (_, _) => controls)
            in
              helpParse (controls, io)
            end
        end
    | NONE => returnControls controls

  fun parse () =
    let
      val initial =
        { left = NONE
        , right = NONE
        , up = NONE
        , down = NONE
        , jump = NONE
        , attack = NONE
        }
      val io = TextIO.openIn "controls.config"
    in
      helpParse (initial, io)
    end
end
