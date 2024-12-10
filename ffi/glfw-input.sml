structure Input =
struct
  type window = MLton.Pointer.t

  (* Constants. *)
  val (PRESS, _) =
    _symbol "PRESS" public : ( unit -> int ) * ( int -> unit );
  val PRESS = PRESS ()

  val (REPEAT, _) =
    _symbol "REPEAT" public : ( unit -> int ) * ( int -> unit );
  val REPEAT = REPEAT ()

  val (RELEASE, _) =
    _symbol "RELEASE" public : ( unit -> int ) * ( int -> unit );
  val RELEASE = RELEASE ()


end
