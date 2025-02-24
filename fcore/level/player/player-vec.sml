structure PlayerVec =
struct
  open EntityType
  open PlayerType

  val walkRightFrames =
   #[ PlayerWalkRight1.lerp
    , PlayerWalkRight2.lerp
    , PlayerWalkRight3.lerp
    , PlayerWalkRight4.lerp
    , PlayerWalkRight5.lerp
    , PlayerWalkRight6.lerp
    , PlayerWalkRight7.lerp
    , PlayerWalkRight8.lerp
    , PlayerWalkRight9.lerp
    , PlayerWalkRight8.lerp
    , PlayerWalkRight7.lerp
    , PlayerWalkRight6.lerp
    , PlayerWalkRight5.lerp
    , PlayerWalkRight4.lerp
    , PlayerWalkRight3.lerp
    , PlayerWalkRight2.lerp
    ]

  val walkLeftFrames =
   #[ PlayerWalkLeft1.lerp
    , PlayerWalkLeft2.lerp
    , PlayerWalkLeft3.lerp
    , PlayerWalkLeft4.lerp
    , PlayerWalkLeft5.lerp
    , PlayerWalkLeft6.lerp
    , PlayerWalkLeft7.lerp
    , PlayerWalkLeft8.lerp
    , PlayerWalkLeft9.lerp
    , PlayerWalkLeft8.lerp
    , PlayerWalkLeft7.lerp
    , PlayerWalkLeft6.lerp
    , PlayerWalkLeft5.lerp
    , PlayerWalkLeft4.lerp
    , PlayerWalkLeft3.lerp
    , PlayerWalkLeft2.lerp
    ]

  fun getIdle (player, rx, ry, dw, dh, ww, wh) =
    case #facing player of
      FACING_RIGHT =>
        PlayerStandingRight.lerp (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    | FACING_LEFT =>
        PlayerStandingLeft.lerp (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)

  fun getWhenOnGround (player, rx, ry, dw, dh, ww, wh) =
    case #xAxis player of
      MOVE_RIGHT =>
        let
          val animTimer = #animTimer player
          val frame = (animTimer div 2) mod Vector.length walkRightFrames
          val func = Vector.sub (walkRightFrames, Int.max (frame, 0))
        in
          func (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
        end
    | MOVE_LEFT =>
        let
          val animTimer = #animTimer player
          val frame = (animTimer div 2) mod Vector.length walkLeftFrames
          val func = Vector.sub (walkLeftFrames, Int.max (frame, 0))
        in
          func (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
        end
    | STAY_STILL => getIdle (player, rx, ry, dw, dh, ww, wh)

  fun getWhenJumpingRight (player, amt, rx, ry, dw, dh, ww, wh) =
    if amt < 3 then
      PlayerJumpRight1.lerp 
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 6 then
      PlayerJumpRight2.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 9 then
      PlayerJumpRight3.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 12 then
      PlayerJumpRight4.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else
      PlayerJumpRight5.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)

  fun getWhenJumpingLeft (player, amt, rx, ry, dw, dh, ww, wh) =
    if amt < 3 then
      PlayerJumpLeft1.lerp 
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 6 then
      PlayerJumpLeft2.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 9 then
      PlayerJumpLeft3.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else if amt < 12 then
      PlayerJumpLeft4.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    else
      PlayerJumpLeft5.lerp
        (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)

  fun getWhenJumping (player, amt, rx, ry, dw, dh, ww, wh) =
    case #facing player of
      FACING_RIGHT => getWhenJumpingRight (player, amt, rx, ry, dw, dh, ww, wh)
    | FACING_LEFT => getWhenJumpingLeft (player, amt, rx, ry, dw, dh, ww, wh)

  fun getWhenFalling (player, rx, ry, dw, dh, ww, wh) =
    case #facing player of
      FACING_RIGHT =>
        PlayerJumpRight5.lerp
          (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
    | FACING_LEFT =>
        PlayerJumpLeft5.lerp
          (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)

  fun getWhenDropping (player, rx, ry, dw, dh, ww, wh) =
    let
      val animTimer = #animTimer player
    in
      getWhenJumping (player, animTimer, rx, ry, dw, dh, ww, wh)
    end

  fun getWhenNotAttacked (player, rx, ry, dw, dh, ww, wh) =
    case #yAxis player of
      ON_GROUND => getWhenOnGround (player, rx, ry, dw, dh, ww, wh)
    | JUMPING amt => getWhenJumping (player, amt, rx, ry, dw, dh, ww, wh)
    | FALLING => getWhenFalling (player, rx, ry, dw, dh, ww, wh)
    | FLOATING _ => getWhenFalling (player, rx, ry, dw, dh, ww, wh)
    | DROP_BELOW_PLATFORM => getWhenDropping (player, rx, ry, dw, dh, ww, wh)

  fun getWhenAttacked (player, amt, rx, ry, dw, dh, ww, wh) =
    case #facing player of
      FACING_RIGHT =>
        if amt mod 5 = 0 then
          PlayerStandingRight.lerp (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
        else
          PlayerStandingRight.lerp (rx, ry, dw, dh, ww, wh, 1.0, 0.75, 0.75)
    | FACING_LEFT =>
        if amt mod 5 = 0 then
          PlayerStandingLeft.lerp (rx, ry, dw, dh, ww, wh, 1.0, 1.0, 1.0)
        else
          PlayerStandingLeft.lerp (rx, ry, dw, dh, ww, wh, 1.0, 0.75, 0.75)

  fun helpGet
    (player: player, rx, ry, drawWidth, drawHeight, windowWidth, windowHeight) =
    case #attacked player of
      NOT_ATTACKED =>
        getWhenNotAttacked
          (player, rx, ry, drawWidth, drawHeight, windowWidth, windowHeight)
    | ATTACKED amt =>
        getWhenAttacked
          ( player
          , amt
          , rx
          , ry
          , drawWidth
          , drawHeight
          , windowWidth
          , windowHeight
          )

  fun get (player: player, width, height) =
    let
      val {x, y, attacked, facing, ...} = player
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

          val realWidth = Constants.playerWidthReal * wratio
          val realHeight = Constants.playerHeightReal * wratio
        in
          helpGet (player, x, y, realWidth, realHeight, width, height)
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

          val realWidth = Constants.playerWidthReal * hratio
          val realHeight = Constants.playerHeightReal * hratio
        in
          helpGet (player, x, y, realWidth, realHeight, width, height)
        end
    end
end
