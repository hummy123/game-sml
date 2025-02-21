structure OptionsVec =
struct
  open OptionsType

  (* There's code duplication here because we want to avoid 
   * branhing if/case expressions per draw-call because of 
   * the branch prediction cost *)
  fun drawLeftKey (options, width, height) =
    let
      val acc =
        if #isSelected options then
          MakeTextVec.make
            (155, 35, width, height, "Left key", 0.7, 0.7, 0.3, [])
        else
          MakeTextVec.make
            (155, 35, width, height, "Left key", 0.3, 0.3, 0.7, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawRightKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawUpKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawDownKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawJumpKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawAttackKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawSaveKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.3, 0.3, 0.7, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.0, 0.0, 0.0, acc)
    in
      Vector.concat acc
    end

  fun drawCancelKey (options, width, height) =
    let
      val acc = MakeTextVec.make
        (155, 35, width, height, "Left key", 0.0, 0.0, 0.0, [])

      val acc = MakeTextVec.make
        (155, 95, width, height, "Right key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 155, width, height, "Up key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 215, width, height, "Down key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 275, width, height, "Jump key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 335, width, height, "Attack key", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 395, width, height, "Save changes", 0.0, 0.0, 0.0, acc)

      val acc = MakeTextVec.make
        (155, 455, width, height, "Cancel changes", 0.3, 0.3, 0.7, acc)
    in
      Vector.concat acc
    end

  fun getDrawVec (options: OptionsType.options_type, width, height) =
    case #focus options of
      LEFT_KEY => drawLeftKey (options, width, height)
    | RIGHT_KEY => drawRightKey (options, width, height)
    | UP_KEY => drawUpKey (options, width, height)
    | DOWN_KEY => drawDownKey (options, width, height)
    | JUMP_KEY => drawJumpKey (options, width, height)
    | ATTACK_KEY => drawAttackKey (options, width, height)
    | SAVE_BUTTON => drawSaveKey (options, width, height)
    | CANCEL_BUTTON => drawCancelKey (options, width, height)
end
