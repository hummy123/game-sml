structure Input =
struct
  type window = MLton.Pointer.t

  (* Constants. *)
  val (PRESS, _) =
    _symbol "PRESS" public : ( unit -> int ) * ( int -> unit );
  val PRESS = PRESS ()

  val (RELEASE, _) =
    _symbol "RELEASE" public : ( unit -> int ) * ( int -> unit );
  val RELEASE = RELEASE ()

  val (ARROW_UP, _) =
    _symbol "ARROW_UP" public : ( unit -> int ) * ( int -> unit );
  val ARROW_UP = ARROW_UP ()

  val (ARROW_DOWN, _) =
    _symbol "ARROW_DOWN" public : ( unit -> int ) * ( int -> unit );
  val ARROW_DOWN = ARROW_DOWN ()

  val (ARROW_LEFT, _) =
    _symbol "ARROW_LEFT" public : ( unit -> int ) * ( int -> unit );
  val ARROW_LEFT = ARROW_LEFT ()

  val (ARROW_RIGHT, _) =
    _symbol "ARROW_RIGHT" public : ( unit -> int ) * ( int -> unit );
  val ARROW_RIGHT = ARROW_RIGHT ()

  val (KEY_S, _) =
    _symbol "KEY_S" public : ( unit -> int ) * ( int -> unit );
  val KEY_S = KEY_S ()

  val (KEY_D, _) =
    _symbol "KEY_D" public : ( unit -> int ) * ( int -> unit );
  val KEY_D = KEY_D ()

  val (KEY_F, _) =
    _symbol "KEY_F" public : ( unit -> int ) * ( int -> unit );
  val KEY_F = KEY_F ()

  val (KEY_J, _) =
    _symbol "KEY_J" public : ( unit -> int ) * ( int -> unit );
  val KEY_J = KEY_J ()

  val (KEY_K, _) =
    _symbol "KEY_K" public : ( unit -> int ) * ( int -> unit );
  val KEY_K = KEY_K ()

  val (KEY_L, _) =
    _symbol "KEY_L" public : ( unit -> int ) * ( int -> unit );
  val KEY_L = KEY_L ()

  val exportKeyCallback =
    _export "mltonKeyCallback" public : (int * int * int * int -> unit) -> unit;
  val setKeyCallback =
    _import "setKeyCallback" public : window -> unit;

  val exportFramebufferSizeCallback =
    _export "mltonFramebufferSizeCallback" public : (Real32.real * Real32.real -> unit) -> unit;
  val setFramebufferSizeCallback =
    _import "setFramebufferSizeCallback" public : window -> unit;
end
