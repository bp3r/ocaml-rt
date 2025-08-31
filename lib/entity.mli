module type Sphere = sig
  type t

  val make : Vec3.t -> float -> Material.t -> t
  (** Make a sphere at the passed origin, with the passed radius, of the passed material. *)

  val center : t -> Vec3.t

  val radius : t -> float

  val material : t -> Material.t
end

module Sphere : Sphere

type t = ESphere of Sphere.t | EList of t list
