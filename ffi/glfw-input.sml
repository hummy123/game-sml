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

  val exportKeyCallback =
    _export "mltonKeyCallback" public : (int * int * int * int -> unit) -> unit;
  val setKeyCallback =
    _import "setKeyCallback" public : window -> unit;

  val exportFramebufferSizeCallback =
    _export "mltonFramebufferSizeCallback" public : (Real32.real * Real32.real -> unit) -> unit;
  val setFramebufferSizeCallback =
    _import "setFramebufferSizeCallback" public : window -> unit;
end
