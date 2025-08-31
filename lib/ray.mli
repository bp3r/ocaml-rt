type t = { origin : Vec3.t; direction : Vec3.t }
(** A ray in 3D space from the camera in some direction. *)

val at : t -> float -> Vec3.t
(** Get the point a ray has reached given an elapsed time. *)

val sample : point:Vec3.t -> span:Vec3.t -> camera_loc:Vec3.t -> t
(** Given some [point], get a ray to a random point within +/- 1/2 the [span] away from it. *)
