structure Field =
struct
  fun lerp (startX, startY, drawWidth, drawHeight, windowWidth, windowHeight
    , r, g, b, a) : Real32.real vector =
    let
       val endY = windowHeight - startY
       val startY = windowHeight - (startY + drawHeight)
       val endX = startX + drawWidth
       val windowHeight = windowHeight / 2.0
       val windowWidth = windowWidth / 2.0
    in
       #[      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b, a,
      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b, a,
      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b, a,
      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b, a,
      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 1.0)) + (endY * 1.0)) / windowHeight) - 1.0, r, g, b, a,
      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.0)) + (endY * 0.0)) / windowHeight) - 1.0, r, g, b, a
    ]
  end
end
