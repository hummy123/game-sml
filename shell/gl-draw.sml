structure GlDraw =
struct
  open CML

  type t = { window: MLton.Pointer.t }

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
      val textVertexBuffer = Gles3.createBuffer ()
      val xyrgbVertexShader = createShader
        (Gles3.VERTEX_SHADER, GlShaders.xyrgbVertexShaderString)

      val rgbFragmentShader = createShader
        (Gles3.FRAGMENT_SHADER, GlShaders.rgbFragmentShaderString)

      val placeholderProgram = createProgram (xyrgbVertexShader, rgbFragmentShader)
    in
      {window = window}
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

  fun helpLoop (shellState as {window, ...}: t) =
    case Glfw.windowShouldClose window of
      false =>
        let
          val _ = Gles3.clearColor (1.0, 1.0, 1.0, 1.0)
          val _ = Gles3.clear ()

          (* todo:
           * - update game state
           * - consume draw messages 
           * - finally, draw 
           * *)
          val _ = Glfw.swapBuffers window
          val _ = Glfw.waitEvents ()
        in
          helpLoop shellState
        end
    | true => Glfw.terminate ()

  fun loop window =
    let val shellState = create window
    in helpLoop shellState
    end
end
