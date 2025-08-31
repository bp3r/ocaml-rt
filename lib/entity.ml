module type Sphere = sig
  type t

  val make : Vec3.t -> float -> Material.t -> t
  (** Make a sphere at the passed origin, with the passed radius, of the passed material. *)

  val center : t -> Vec3.t

  val radius : t -> float

  val material : t -> Material.t
end

module Sphere : Sphere = struct
  type t = { center : Vec3.t; radius : float; material : Material.t }

  let make center radius material =
    assert (radius > 0.0);
    { center; radius; material }

  let center { center; _ } = center

  let radius { radius; _ } = radius

  let material { material; _ } = material
end

type t = ESphere of Sphere.t | EList of t list
