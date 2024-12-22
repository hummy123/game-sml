structure InputState =
struct
  (* global state detecting button inputs *)
  val state =
    { leftHeld = ref false
    , rightHeld = ref false
    , upHeld = ref false
    , downHeld = ref false
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
    }

  fun getWidth () =
    !(#width state)

  fun getHeight () =
    !(#height state)

  fun sizeCallback (width, height) =
    (#width state := width; #height state := height)

  open Input

  fun handleKey (key, action) =
    if key = KEY_K then
      if action = PRESS then (#upHeld state) := true
      else if action = RELEASE then (#upHeld state) := false
      else ()
    else if key = KEY_D then
      if action = PRESS then (#downHeld state) := true
      else if action = RELEASE then (#downHeld state) := false
      else ()
    else if key = KEY_S then
      if action = PRESS then (#leftHeld state) := true
      else if action = RELEASE then (#leftHeld state) := false
      else ()
    else if key = KEY_F then
      if action = PRESS then (#rightHeld state) := true
      else if action = RELEASE then (#rightHeld state) := false
      else ()
    else if key = KEY_J then
      if action = PRESS then (#attackHeld state) := true
      else if action = RELEASE then (#attackHeld state) := false
      else ()
    else
      ()

  fun keyCallback (key, scancode, action, mods) =
    let open Input
    in if mods = 0 then handleKey (key, action) else ()
    end

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
