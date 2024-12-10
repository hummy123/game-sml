structure Glfw =
struct
  type window = MLton.Pointer.t

  (* Window hint constants. *)
  val (CONTEXT_VERSION_MAJOR, _) =
    _symbol "CONTEXT_VERSION_MAJOR" public : ( unit -> int ) * ( int -> unit );
  val (DEPRECATED, _) =
    _symbol "DEPRECATED" public : ( unit -> int ) * ( int -> unit );
  val (TRUE, _) =
    _symbol "GLFW_FFI_TRUE" public : ( unit -> int ) * ( int -> unit );
  val (FALSE, _) =
    _symbol "GLFW_FFI_FALSE" public : ( unit -> int ) * ( int -> unit );
  val (SAMPLES, _) =
    _symbol "SAMPLES" public : ( unit -> int ) * ( int -> unit );
  val (WINDOW_MAX, _) =
    _symbol "GLFW_WINDOW_MAX" public : ( unit -> int ) * ( int -> unit );

  (* GLFW functions. *)
  val init = _import "init" public : unit -> unit;
  val windowHint = _import "windowHint" public : int * int -> unit;
  val createWindow =
    _import "createWindow" public : int * int * string -> window;
  val terminate = _import "terminate" public : unit -> unit;
  val makeContextCurrent = _import "makeContextCurrent" public : window -> unit;
  val windowShouldClose = _import "windowShouldClose" public : window -> bool;
  val waitEvents = _import "waitEvents" public reentrant : unit -> unit;
  val swapBuffers = _import "swapBuffers" public : window -> unit;
  val setClipboardString = _import "setClipboardString" public : window * string -> unit;
end
