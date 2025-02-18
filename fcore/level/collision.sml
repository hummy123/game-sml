structure Collision =
struct
  fun isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy) =
    ix < cfx andalso ifx > cx andalso iy < cfy andalso ify > cy

  fun isCollidingPlus (ix, iy, iw, ih, cx, cy, cw, ch) =
    let
      val ifx = ix + iw
      val ify = iy + ih
      val cfx = cx + cw
      val cfy = cy + ch
    in
      isColliding (ix, iy, ifx, ify, cx, cy, cfx, cfy)
    end
end
