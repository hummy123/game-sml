structure GlDraw =
struct
  type t =
    { window: MLton.Pointer.t
    , wallVertexBuffer: Word32.word
    , wallProgram: Word32.word
    , wallLength: int
    , playerVertexBuffer: Word32.word
    , playerProgram: Word32.word
    , playerLength: int
    , fieldVertexBuffer: Word32.word
    , fieldProgram: Word32.word
    , fieldLength: int
    }

  fun createShader (shaderType, shaderString) =
    let
      val shader = Gles3.createShader shaderType
      val _ = Gles3.shaderSource (shader, shaderString)
      val _ = Gles3.compileShader shader
    in
      shader
    end

  fun createProgram (vertexShader, fragmentShader) =
    let
      val program = Gles3.createProgram ()
      val _ = Gles3.attachShader (program, vertexShader)
      val _ = Gles3.attachShader (program, fragmentShader)
      val _ = Gles3.linkProgram program
    in
      program
    end

  fun create window =
    let
      (* create vertex buffer, program, etc. *)
      val xyrgbVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyrgbVertexShaderString)

      val rgbFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbFragmentShaderString)

      val xyrgbaVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyrgbaVertexShaderString)

      val rgbaFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbaFragmentShaderString)

      (* wall here includes both walls and platforms *)
      val wallVertexBuffer = Gles3.createBuffer ()
      val wallProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      val playerVertexBuffer = Gles3.createBuffer ()
      val playerProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)

      val fieldVertexBuffer = Gles3.createBuffer ()
      val fieldProgram = createProgram (xyrgbaVertexShader, rgbaFragmentShader)
    in
      { window = window
      , wallVertexBuffer = wallVertexBuffer
      , wallProgram = wallProgram
      , wallLength = 0
      , playerVertexBuffer = playerVertexBuffer
      , playerProgram = playerProgram
      , playerLength = 0
      , fieldVertexBuffer = fieldVertexBuffer
      , fieldProgram = fieldProgram
      , fieldLength = 0
      }
    end

  fun uploadWall (shellState: t, vec) =
    let
      val
        { window
        , playerVertexBuffer
        , playerProgram
        , playerLength
        , fieldVertexBuffer
        , fieldProgram
        , fieldLength
        , wallVertexBuffer
        , wallProgram
        , wallLength = _
        } = shellState

      val _ = Gles3.bindBuffer wallVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newWallLength = Vector.length vec div 5
    in
      { window = window
      , playerVertexBuffer = playerVertexBuffer
      , playerProgram = playerProgram
      , playerLength = playerLength
      , fieldVertexBuffer = fieldVertexBuffer
      , fieldProgram = fieldProgram
      , fieldLength = fieldLength
      , wallVertexBuffer = wallVertexBuffer
      , wallProgram = wallProgram
      , wallLength = newWallLength
      }
    end

  fun uploadPlayer (shellState: t, vec) =
    let
      val
        { window
        , wallVertexBuffer
        , wallProgram
        , wallLength
        , fieldVertexBuffer
        , fieldProgram
        , fieldLength
        , playerVertexBuffer
        , playerProgram
        , playerLength = _
        } = shellState

      val _ = Gles3.bindBuffer playerVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newPlayerLength = Vector.length vec div 5
    in
      { window = window
      , wallVertexBuffer = wallVertexBuffer
      , wallProgram = wallProgram
      , wallLength = wallLength
      , fieldVertexBuffer = fieldVertexBuffer
      , fieldProgram = fieldProgram
      , fieldLength = fieldLength
      , playerVertexBuffer = playerVertexBuffer
      , playerProgram = playerProgram
      , playerLength = newPlayerLength
      }
    end

  fun uploadField (shellState: t, vec) =
    let
      val
        { window
        , wallVertexBuffer
        , wallProgram
        , wallLength
        , playerVertexBuffer
        , playerProgram
        , playerLength
        , fieldVertexBuffer
        , fieldProgram
        , fieldLength = _
        } = shellState

      val _ = Gles3.bindBuffer fieldVertexBuffer
      val _ = Gles3.bufferData (vec, Vector.length vec, Gles3.STATIC_DRAW)
      val newFieldLength = Vector.length vec div 6
    in
      { window = window
      , wallVertexBuffer = wallVertexBuffer
      , wallProgram = wallProgram
      , wallLength = wallLength
      , playerVertexBuffer = playerVertexBuffer
      , playerProgram = playerProgram
      , playerLength = playerLength
      , fieldVertexBuffer = fieldVertexBuffer
      , fieldProgram = fieldProgram
      , fieldLength = newFieldLength
      }
    end

  fun drawXyrgb (vertexBuffer, program, drawLength) =
    if drawLength > 0 then
      let
        val _ = Gles3.bindBuffer vertexBuffer
        (* enable xy component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (0, 2, 5, 0)
        val _ = Gles3.enableVertexAttribArray 0
        (* enable rgb component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (1, 3, 5, 8)
        val _ = Gles3.enableVertexAttribArray 1

        val _ = Gles3.useProgram program
        val _ = Gles3.drawArrays (Gles3.TRIANGLES, 0, drawLength)
      in
        ()
      end
    else
      ()

  fun drawXyrgba (vertexBuffer, program, drawLength) =
    if drawLength > 0 then
      let
        val _ = Gles3.bindBuffer vertexBuffer
        (* enable xy component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (0, 2, 6, 0)
        val _ = Gles3.enableVertexAttribArray 0
        (* enable rgb component from uploaded array *)
        val _ = Gles3.vertexAttribPointer (1, 4, 6, 8)
        val _ = Gles3.enableVertexAttribArray 1

        val _ = Gles3.useProgram program
        val _ = Gles3.drawArrays (Gles3.TRIANGLES, 0, drawLength)
      in
        ()
      end
    else
      ()

  fun drawWall ({wallVertexBuffer, wallProgram, wallLength, ...}: t) =
    drawXyrgb (wallVertexBuffer, wallProgram, wallLength)

  fun drawPlayer ({playerVertexBuffer, playerProgram, playerLength, ...}: t) =
    drawXyrgb (playerVertexBuffer, playerProgram, playerLength)

  fun drawField ({fieldVertexBuffer, fieldProgram, fieldLength, ...}) =
    drawXyrgba (fieldVertexBuffer, fieldProgram, fieldLength)

  fun helpDrawLevel (shellState: t) =
    let
      val _ = drawWall shellState
      val _ = drawPlayer shellState
      val _ = drawField shellState
    in
      ()
    end

  fun drawLevel (shellState: t, level) =
    let
      val width = InputState.getWidth ()
      val height = InputState.getHeight ()

      val playerVec = Player.getDrawVec (#player level, width, height)
      val enemyVec = Enemy.getDrawVec (#enemies level, width, height)
      val playerVec = Vector.concat [playerVec, enemyVec]

      val wallVec = Wall.getDrawVec (#walls level, width, height)
      val platVec = Platform.getDrawVec (#platforms level, width, height)
      val chainVec = Player.getFieldVec (#player level, width, height)
      val fallingVec = FallingEnemies.getDrawVec (level, width, height)
      val wallVec = Vector.concat [wallVec, platVec, chainVec, fallingVec]

      val pelletVec = Player.getPelletVec (#player level, width, height)
      val projectileVec =
        Projectile.getProjectileVec (#player level, width, height)
      val fieldVec = Vector.concat [pelletVec, projectileVec]

      val shellState = uploadWall (shellState, wallVec)
      val shellState = uploadPlayer (shellState, playerVec)
      val shellState = uploadField (shellState, fieldVec)
      val () = helpDrawLevel shellState
    in
      shellState
    end

  fun helpDrawTitle (shellState: t) = drawPlayer shellState

  fun drawTitle (shellState: t, title) =
    let
      val width = InputState.getWidth ()
      val height = InputState.getHeight ()
      val vec = TitleVec.getDrawVec (title, width, height)
      val shellState = uploadPlayer (shellState, vec)
      val () = helpDrawTitle shellState
    in
      shellState
    end

  fun helpDrawOptions shellState = drawPlayer shellState

  fun drawOptions (shellState: t, options) =
    let
      val width = InputState.getWidth ()
      val height = InputState.getHeight ()
      val vec = OptionsVec.getDrawVec (options, width, height)
      val shellState = uploadPlayer (shellState, vec)
      val () = helpDrawOptions shellState
    in
      shellState
    end

  fun drawMode (shellState: t, game: GameType.game_type) =
    let
      open GameType
    in
      case #mode game of
        LEVEL level => drawLevel (shellState, level)
      | TITLE title => drawTitle (shellState, title)
      | OPTIONS options => drawOptions (shellState, options)
    end

  fun helpLoop (shellState as {window, ...}: t, game) =
    case Glfw.windowShouldClose window of
      false =>
        let
          val _ = Gles3.clearColor (1.0, 1.0, 1.0, 1.0)
          val _ = Gles3.clear ()

          val input = InputState.getSnapshot ()
          val game = GameUpdate.update (game, input)

          val shellState = drawMode (shellState, game)

          val _ = Glfw.swapBuffers window
          val _ = Glfw.pollEvents ()
        in
          helpLoop (shellState, game)
        end
    | true => Glfw.terminate ()

  fun loop window =
    let
      val shellState = create window
      val controls =
        case ParseControls.parse () of
          SOME controls => controls
        | NONE =>
            { left = CoreKey.KEY_LEFT
            , right = CoreKey.KEY_RIGHT
            , up = CoreKey.KEY_UP
            , down = CoreKey.KEY_DOWN
            , jump = CoreKey.KEY_Z
            , attack = CoreKey.KEY_X
            , escape = CoreKey.KEY_ESCAPE
            }

      val () = InputState.setControls controls
    in
      helpLoop (shellState, GameType.init controls)
    end
end
