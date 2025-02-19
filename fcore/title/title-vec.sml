structure TitleVec =
struct
  fun helpGetTextVec
    (x, y, fontSize, fontSpace, windowWidth, windowHeight, pos, str, acc) =
    if pos = String.size str then
      Vector.concat acc
    else
      let
        val chr = String.sub (str, pos)
        val chrFun = Vector.sub (CozetteAscii.asciiTable, Char.ord chr)

        val hd = chrFun
          (x, y, fontSize, fontSize, windowWidth, windowHeight, 0.0, 0.0, 0.0)
        val acc = hd :: acc
      in
        helpGetTextVec
          ( x + fontSpace
          , y
          , fontSize
          , fontSpace
          , windowWidth
          , windowHeight
          , pos + 1
          , str
          , acc
          )
      end

  fun getTextWidth text =
    String.size text * Constants.fontSpace

  (* x coordinate that will let us place this text on centre of screen *)
  fun getTextCentreX text =
    let
      val textWidth = getTextWidth text
    in
      (Constants.worldWidth - textWidth) div 2
    end

  fun getTextVec (x, y, width, height, str) =
    let
      val wratio = width / Constants.worldWidthReal
      val hratio = height / Constants.worldHeightReal
    in
      if wratio < hratio then
        let
          val scale = Constants.worldHeightReal * wratio
          val yOffset =
            if height > scale then (height - scale) / 2.0
            else if height < scale then (scale - height) / 2.0
            else 0.0

          val x = Real32.fromInt x * wratio
          val y = Real32.fromInt y * wratio + yOffset

          val x = Real32.toInt IEEEReal.TO_NEAREST x
          val y = Real32.toInt IEEEReal.TO_NEAREST y

          val fontSize = Constants.fontSize * wratio

          val fontSpace = Real32.fromInt Constants.fontSpace * wratio
          val fontSpace = Real32.toInt IEEEReal.TO_NEAREST fontSpace
        in
          helpGetTextVec (x, y, fontSize, fontSpace, width, height, 0, str, [])
        end
      else
        let
          val scale = Constants.worldWidthReal * hratio
          val xOffset =
            if width > scale then (width - scale) / 2.0
            else if width < scale then (scale - width) / 2.0
            else 0.0

          val x = Real32.fromInt x * hratio + xOffset
          val y = Real32.fromInt y * hratio

          val x = Real32.toInt IEEEReal.TO_NEAREST x
          val y = Real32.toInt IEEEReal.TO_NEAREST y

          val fontSize = Constants.fontSize * hratio

          val fontSpace = Real32.fromInt Constants.fontSpace * hratio
          val fontSpace = Real32.toInt IEEEReal.TO_NEAREST fontSpace
        in
          helpGetTextVec (x, y, fontSize, fontSpace, width, height, 0, str, [])
        end
    end

  fun getDrawVec (title: TitleType.title_type, width, height) =
    let
      val playX = getTextCentreX "hello world"
    in
      getTextVec (playX, 500, width, height, "hello world")
    end
end
