type t = float * float * float

let epsilon = 1e-20 (* Used to limit the occurrence of roundoff error. *)

let ( %.% ) (x1, y1, z1) (x2, y2, z2) = (x1 *. x2) +. (y1 *. y2) +. (z1 *. z2)

let ( %+% ) (x1, y1, z1) (x2, y2, z2) = (x1 +. x2, y1 +. y2, z1 +. z2)

let ( %*% ) (x1, y1, z1) (x2, y2, z2) = (x1 *. x2, y1 *. y2, z1 *. z1)

let ( *% ) f (x, y, z) = (f *. x, f *. y, f *. z)

let negate (x, y, z) = (-.x, -.y, -.z)

let ( %-% ) v w = v %+% negate w

let length v = sqrt (v %.% v)

let normalize v = 1. /. length v *% v

let random () =
  let f () = Random.float 2. -. 1. in
  (f (), f (), f ())

let cross (x1, y1, z1) (x2, y2, z2) =
  ((y1 *. z2) -. (z1 *. y2), (z1 *. x2) -. (x1 *. z2), (x1 *. y2) -. (y1 *. x2))

let rec random_to_unit_sphere () =
  (* We generate a vector in the unit sphere before normalizing it to ensure we don't bias
     generation to favour certain directions, which would happen if we used the cube around it
     instead. *)
  let v = random () in
  let v_length_squared = v %.% v in
  if v_length_squared <= 1. && v_length_squared > epsilon then normalize v
  else random_to_unit_sphere ()

(** Checks a condition on all of a vector's components. *)
let all f (x, y, z) = f x && f y && f z

let is_near_zero v = all (fun e -> e < epsilon) v

type lerp_function = float -> t

let lerp start finish s =
  assert (s >= 0. && s <= 1.);
  (1. -. s) *% start %+% (s *% finish)

let reflect ~v ~n = v %-% (2. *. (v %.% n) *% n)

let refract ~uv ~n ~index_ratio =
  (* Both refraction and reflection occur in 2D along the plane spanned by the incoming ray and the
     surface normal. We can thus split the direction of the refracted ray into horizontal and
     vertical components and calculate each separately. The following is derived from Snell's law. *)
  let cos_theta = min (negate uv %.% n) 1. in
  let incident_perp = index_ratio *% (uv %+% (cos_theta *% n)) in
  let incident_para = -.sqrt (abs_float (1. -. (incident_perp %.% incident_perp))) *% n in
  incident_perp %+% incident_para

let pp (fmt : Format.formatter) ((x, y, z) : t) : unit = Format.fprintf fmt "[%f %f %f]\n" x y z
