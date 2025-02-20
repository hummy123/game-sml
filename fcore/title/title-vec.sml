structure TitleVec =
struct
  open TitleType

  fun getDrawVec (title: TitleType.title_type, width, height) =
    case #focus title of
      START_BUTTON =>
        let
          val playX = MakeTextVec.getTextCentreX "Play game"
          val acc = MakeTextVec.make
            (playX, 500, width, height, "Play game", 0.3, 0.3, 0.7, [])

          val optionsX = MakeTextVec.getTextCentreX "Options"
          val acc = MakeTextVec.make
            (optionsX, 600, width, height, "Options", 0.0, 0.0, 0.0, acc)
        in
          Vector.concat acc
        end
    | OPTIONS_BUTTON =>
        let
          val playX = MakeTextVec.getTextCentreX "Play game"
          val acc = MakeTextVec.make
            (playX, 500, width, height, "Play game", 0.0, 0.0, 0.0, [])

          val optionsX = MakeTextVec.getTextCentreX "Options"
          val acc = MakeTextVec.make
            (optionsX, 600, width, height, "Options", 0.3, 0.3, 0.7, acc)
        in
          Vector.concat acc
        end
end
