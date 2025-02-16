structure InputState =
struct
  val keyMappings = ref
    { left = CoreKey.KEY_S
    , right = CoreKey.KEY_F
    , down = CoreKey.KEY_D
    , up = CoreKey.KEY_E
    , attack = CoreKey.KEY_J
    , jump = CoreKey.KEY_K
    }

  (* global state detecting button inputs *)
  val state =
    { leftHeld = ref false
    , rightHeld = ref false
    , upHeld = ref false
    , downHeld = ref false
    , jumpHeld = ref false
    , attackHeld = ref false
    , width = ref (1920.0 : Real32.real)
    , height = ref (1080.0 : Real32.real)
    }

  fun getSnapshot () =
    { leftHeld = !(#leftHeld state)
    , rightHeld = !(#rightHeld state)
    , upHeld = !(#upHeld state)
    , downHeld = !(#downHeld state)
    , attackHeld = !(#attackHeld state)
    , jumpHeld = !(#jumpHeld state)
    }

  fun actionToBool action = action = Input.PRESS

  fun handleKey (key, action) =
    case GlfwKeyMap.codeFromKey key of
      SOME code =>
        let
          val {left, right, down, up, attack, jump} = !keyMappings
          val action = actionToBool action
        in
          if code = left then #leftHeld state := action
          else if code = up then #upHeld state := action
          else if code = right then #rightHeld state := action
          else if code = down then #downHeld state := action
          else if code = attack then #attackHeld state := action
          else if code = jump then #jumpHeld state := action
          else ()
        end
    | NONE => ()

  fun keyCallback (key, scancode, action, mods) =
    let open Input
    in if mods = 0 then handleKey (key, action) else ()
    end

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
