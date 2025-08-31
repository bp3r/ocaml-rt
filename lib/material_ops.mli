val scatter : Ray.t -> Hit.t -> Ray.t option
(** Scatters a ray as it hits an entity of some material. Returns a new ray if the old one was
    scattered (rather than absorbed by the material). *)

val albedo : Material.t -> Vec3.t
