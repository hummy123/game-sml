signature INPUT_MSG =
sig
  datatype t =
    RESIZE_WINDOW of {width: int, height: int}
end

structure InputMsg =
struct
  datatype t =
    RESIZE_WINDOW of {width: int, height: int}
end
