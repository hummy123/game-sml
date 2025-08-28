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
    ]

  fun getIdle (player, rx, ry, ww, wh) =
    case #facing player of
      FACING_RIGHT => PlayerStandingRight.lerp (rx, ry, 3.0, ww, wh)
    | FACING_LEFT => PlayerStandingLeft.lerp (rx, ry, 3.0, ww, wh)

  fun getWalk (rx, ry, ww, wh, walkFrames, animTimer) =
    let
      val frame = (animTimer div 4) mod Vector.length walkFrames
      val func = Vector.sub (walkFrames, Int.max (frame, 0))
    in
      func (rx, ry, 3.0, ww, wh)
    end

  fun getWhenOnGround (player, rx, ry, ww, wh) =
    case #xAxis player of
      MOVE_RIGHT => getWalk (rx, ry, ww, wh, walkRightFrames, #animTimer player)
    | MOVE_LEFT => getWalk (rx, ry, ww, wh, walkLeftFrames, #animTimer player)
    | STAY_STILL => getIdle (player, rx, ry, ww, wh)

  fun getWhenJumpingRight (player, amt, rx, ry, ww, wh) =
    if amt < 3 then PlayerJumpRight1.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 6 then PlayerJumpRight2.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 9 then PlayerJumpRight3.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 12 then PlayerJumpRight4.lerp (rx, ry, 3.0, ww, wh)
    else PlayerJumpRight5.lerp (rx, ry, 3.0, ww, wh)

  fun getWhenJumpingLeft (player, amt, rx, ry, ww, wh) =
    if amt < 3 then PlayerJumpLeft1.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 6 then PlayerJumpLeft2.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 9 then PlayerJumpLeft3.lerp (rx, ry, 3.0, ww, wh)
    else if amt < 12 then PlayerJumpLeft4.lerp (rx, ry, 3.0, ww, wh)
    else PlayerJumpLeft5.lerp (rx, ry, 3.0, ww, wh)

  fun getWhenJumping (player, amt, rx, ry, ww, wh) =
    case #facing player of
      FACING_RIGHT => getWhenJumpingRight (player, amt, rx, ry, ww, wh)
    | FACING_LEFT => getWhenJumpingLeft (player, amt, rx, ry, ww, wh)

  fun getWhenFalling (player, rx, ry, ww, wh) =
    case #facing player of
      FACING_RIGHT => PlayerJumpRight5.lerp (rx, ry, 3.0, ww, wh)
    | FACING_LEFT => PlayerJumpLeft5.lerp (rx, ry, 3.0, ww, wh)

  fun getWhenDropping (player, rx, ry, ww, wh) =
    let val animTimer = #animTimer player
    in getWhenJumping (player, animTimer, rx, ry, ww, wh)
    end

  fun getWhenNotAttacked (player, rx, ry, ww, wh) =
    case #yAxis player of
      ON_GROUND => getWhenOnGround (player, rx, ry, ww, wh)
    | JUMPING amt => getWhenJumping (player, amt, rx, ry, ww, wh)
    | FALLING => getWhenFalling (player, rx, ry, ww, wh)
    | FLOATING _ => getWhenFalling (player, rx, ry, ww, wh)
    | DROP_BELOW_PLATFORM => getWhenDropping (player, rx, ry, ww, wh)

  fun getWhenAttacked (player, amt, rx, ry, ww, wh) =
    case #facing player of
      FACING_RIGHT =>
        (* todo: hurt sprite/animation if amt mod 5 = 0 then *)
        PlayerStandingRight.lerp (rx, ry, 3.0, ww, wh)
    | FACING_LEFT =>
        (* todo: hurt sprite/animation if amt mod 5 = 0 then *)
        PlayerStandingLeft.lerp (rx, ry, 3.0, ww, wh)

  fun helpGet (player: player, rx, ry, windowWidth, windowHeight) =
    case #mainAttack player of
      MAIN_ATTACKING amt =>
        let
          val playerVec = PlayerAttackStandLeft.lerp (rx, ry, 3.0, windowWidth, windowHeight)
          val projectileVec = Vector.fromList []
        in
          playerVec
        end
    | _ =>
        case #attacked player of
          NOT_ATTACKED =>
            getWhenNotAttacked (player, rx, ry, windowWidth, windowHeight)
        | ATTACKED amt =>
            getWhenAttacked (player, amt, rx, ry, windowWidth, windowHeight)

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
        in
          helpGet (player, x, y, width, height)
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
        in
          helpGet (player, x, y, width, height)
        end
    end
end
