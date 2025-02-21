structure InputState =
struct
  val keyMappings = ref
    { left = CoreKey.KEY_S
    , right = CoreKey.KEY_F
    , down = CoreKey.KEY_D
    , up = CoreKey.KEY_E
    , attack = CoreKey.KEY_J
    , jump = CoreKey.KEY_K
    , escape = CoreKey.KEY_ESCAPE
    }

  fun setControls controls = keyMappings := controls

  (* global state detecting button inputs *)
  val state =
    { leftHeld = ref false
    , rightHeld = ref false
    , upHeld = ref false
    , downHeld = ref false
    , jumpHeld = ref false
    , attackHeld = ref false
    , escapeHeld = ref false
    , newKeys = ref []
    , width = ref (1920.0 : Real32.real)
    , height = ref (1080.0 : Real32.real)
    }

  fun getSnapshot () =
    let
      val input =
        { leftHeld = !(#leftHeld state)
        , rightHeld = !(#rightHeld state)
        , upHeld = !(#upHeld state)
        , downHeld = !(#downHeld state)
        , attackHeld = !(#attackHeld state)
        , jumpHeld = !(#jumpHeld state)
        , escapeHeld = !(#escapeHeld state)
        , newKeys = !(#newKeys state)
        }
      val () = #newKeys state := []
    in
      input
    end

  (* there are three action states reported by OS: PRESS, REPEAT and RELEASE. 
   * If input is PRESS or REPEAT, then return true, or else return false. *)
  fun actionToBool action = action <> Input.RELEASE

  fun handleKey (key, action) =
    case GlfwKeyMap.codeFromKey key of
      SOME code =>
        let
          val () = #newKeys state := code :: !(#newKeys state)
          val {left, right, down, up, attack, jump, escape} = !keyMappings
          val action = actionToBool action
        in
          if code = left then #leftHeld state := action
          else if code = up then #upHeld state := action
          else if code = right then #rightHeld state := action
          else if code = down then #downHeld state := action
          else if code = attack then #attackHeld state := action
          else if code = jump then #jumpHeld state := action
          else if code = escape then #escapeHeld state := action
          else ()
        end
    | NONE => ()

  fun keyCallback (key, scancode, action, mods) =
    if mods = 0 then handleKey (key, action) else ()

  fun getWidth () =
    !(#width state)

  fun getHeight () =
    !(#height state)

  fun sizeCallback (width, height) =
    (#width state := width; #height state := height)

  fun registerCallbacks window =
    let
      val () = Input.exportKeyCallback keyCallback
      val () = Input.setKeyCallback window

      val () = Input.exportFramebufferSizeCallback sizeCallback
      val () = Input.setFramebufferSizeCallback window
    in
      ()
    end
end
