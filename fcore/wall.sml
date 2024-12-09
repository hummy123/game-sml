structure Wall =
struct
  (* Wall or platform, where player can land after falling. 
   * Difference between wall and platform is that one can jump to a platform 
   * and go below it, but wall is completely opaque. *)
  datatype wall_type = WALL | PLATFORM


end
