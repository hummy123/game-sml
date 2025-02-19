structure TitleVec =
struct
  val fontSpace = Constants.fontSpace
  val fontSize = Constants.fontSize

  fun getTextVec (x, y, windowWidth, windowHeight, pos, str, acc) =
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
        getTextVec
          (x + fontSpace, y, windowWidth, windowHeight, pos + 1, str, acc)
      end

  fun getDrawVec (title: TitleType.title_type, width, height) =
    getTextVec (555, 55, width, height, 0, "hello world", [])
end
