type t

val make : float -> float -> float -> t
(** Create a new Color from the passed RGB values; each will be constrained to be in the range
    [[0, 1]]. *)

val ( %*% ) : Vec3.t -> t -> t
(** Scale the color by a vector, channel by channel. *)

val ( %+% ) : t -> t -> t
(** Add two colors together. *)

val ( *% ) : float -> t -> t
(** Scale a color by a scalar. *)  

val black : t

val write_ppm : t -> Out_channel.t -> unit
(** Output the passed [Color.t] to the passed open channel in PPM format. *)
