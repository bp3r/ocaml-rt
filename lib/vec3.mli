type t = float * float * float
(** [Vec3]s are used to represent both vectors and points. *)

val ( %.% ) : t -> t -> float
(** The dot product. *)

val ( %*% ) : t -> t -> t
(** The elementwise product. *)

val ( %+% ) : t -> t -> t
(** The elementwise sum. *)

val ( *% ) : float -> t -> t
(** The scalar multiple of a vector. *)

val negate : t -> t
(** Negate a vector. *)

val ( %-% ) : t -> t -> t
(** Subtract one vector from another. *)

val cross : t -> t -> t
(** The cross product. *)  

val length : t -> float
(** Get the 2-norm of a vector. *)

val normalize : t -> t
(** Normalize a vector to unit length. *)

val random : unit -> t
(** Get a random vector where each element is in the range [[-1, 1]]. *)

val random_to_unit_sphere : unit -> t
(** Get a random vector that touches the surface of the unit sphere (at the origin). *)

val lerp : t -> t -> float -> t
(** Linearly interpolate between a start and finish vector given a scalar in the range [[0, 1]]. *)

val is_near_zero : t -> bool
(** Returns whether a vector is close to 0 in all dimensions. Used to limit the occurrence of
    roundoff error. *)

type lerp_function = float -> t
(** A [lerp_function] that produces a vector given some scalar. *)

val reflect : v:t -> n:t -> t
(** The (orthogonal) reflection of a vector given the normal [n] of some surface that has been hit.
    [n] must be of unit length; this is not checked, for efficiency. *)

val refract : uv:t -> n:t -> index_ratio:float -> t
(** Refracts a unit vector given the normal of some surface that has been hit. Both the vector and
    the normal must be of unit length; this is not checked, for efficiency. [index_ratio] is the
    ratio of the refractive index of the material that has been traversed to the material that is
    being hit. *)

val pp : Format.formatter -> t -> unit
