structure ChainEdgeRight =
struct
  fun lerp (startX, startY, drawWidth, drawHeight, windowWidth, windowHeight, r, g, b) : Real32.real vector =
    let
       val endY = windowHeight - startY
       val startY = windowHeight - (startY + drawHeight)
       val endX = startX + drawWidth
       val windowHeight = windowHeight / 2.0
       val windowWidth = windowWidth / 2.0
    in
       #[      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.799999952316)) + (endY * 0.799999952316)) / windowHeight) - 1.0, 
      r, g, b,

      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.200000017881)) + (endY * 0.200000017881)) / windowHeight) - 1.0,
      r, g, b,

      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.485714316368)) + (endY * 0.485714316368)) / windowHeight) - 1.0, 
      r, g, b
    ]
  end
end

structure ChainEdgeLeft =
struct
  fun lerp (startX, startY, drawWidth, drawHeight, windowWidth, windowHeight, r, g, b) : Real32.real vector =
    let
       val endY = windowHeight - startY
       val startY = windowHeight - (startY + drawHeight)
       val endX = startX + drawWidth
       val windowHeight = windowHeight / 2.0
       val windowWidth = windowWidth / 2.0
    in
       #[      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.799999952316)) + (endY * 0.799999952316)) / windowHeight) - 1.0, 
      r, g, b,

      (((startX * (1.0 - 1.0)) + (endX * 1.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.200000017881)) + (endY * 0.200000017881)) / windowHeight) - 1.0, 
      r, g, b,

      (((startX * (1.0 - 0.0)) + (endX * 0.0)) / windowWidth) - 1.0,
      (((startY * (1.0 - 0.485714316368)) + (endY * 0.485714316368)) / windowHeight) - 1.0, 
      r, g, b
    ]
  end
end
