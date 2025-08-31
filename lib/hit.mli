type t = {
  point : Vec3.t;  (** The point at which the hit occurred. *)
  unit_normal : Vec3.t;
      (** The normal to the hit. Must always point against the ray that caused the hit and must be
          of unit length - these conditions are unchecked on instantiation for efficiency reasons.
      *)
  time : float;  (** The time at which the hit occurred. *)
  is_inside_geometry : bool;
      (** Whether the hit occurred inside the geometry of an entity or not. *)
  material : Material.t;  (** The material of the entity that was hit. *)
}

val get : Ray.t -> Entity.t -> Interval.t -> t option
(** Calculate whether a ray hits an entity (or list of entities) inside a time interval and get
    details about the hit if so. *)
