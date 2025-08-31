module type Position_endpoints = sig
  type t = { start : Vec3.t; finish : Vec3.t }

  val parse : Yojson_five.Basic.t -> t
  (** Parse the provided pair of vector positions into their native representations. *)
end

module Position_endpoints : Position_endpoints

type t = {
  filename_prefix : string;
  n_frames : int;
  fps : int;
  fov_degrees : float;
  image_width : int;
  aspect_ratio : float;
  thread_count : int;
  samples_per_pixel : int;
  max_ray_rebounds : int;
  camera : Position_endpoints.t;
  focus : Position_endpoints.t;
  world : Entity.t;
}

val parse : string -> t
(** Parse the JSON configuration at the provided filepath into a config record. *)
