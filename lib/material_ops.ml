open Material

let albedo = function
  | Lambertian { albedo } -> albedo
  | Metal { albedo; _ } -> albedo
  | Dielectric _ -> (1., 1., 1.)

(** Use Shlick's approximation to calculate the reflectance of a dielectric material. The greater
    the reflectance (up to 1), the greater the proportion of light reflected by the material; more
    light is reflected at greater angles from the surface (see [scatter]). [cos_theta] is the cosine
    of the angle from the surface normal to the incoming ray. [index_ratio] is the ratio of the
    refractive index of the material previously traversed to the material being hit. *)
let reflectance cos_theta index_ratio =
  let r0 = (1. -. index_ratio) /. (1. +. index_ratio) in
  (r0 ** 2.) +. ((1. -. (r0 ** 2.)) *. ((1. -. cos_theta) ** 5.))

let scatter (ray : Ray.t) (hit : Hit.t) : Ray.t option =
  let open Vec3 in
  match hit.material with
  | Lambertian _ ->
      let reflect_direction = hit.unit_normal %+% random_to_unit_sphere () in
      Some
        {
          origin = hit.point;
          direction =
            (if is_near_zero reflect_direction then hit.unit_normal else reflect_direction);
        }
  | Metal { fuzziness; _ } ->
      let reflect_direction = reflect ~v:ray.direction ~n:hit.unit_normal in
      let fuzz_direction = fuzziness *% random_to_unit_sphere () in
      (* At high fuzziness, there's a chance the reflection ends up back inside the geometry.
         In that case we say that the ray is fully absorbed. *)
      if not hit.is_inside_geometry then
        Some { origin = hit.point; direction = fuzz_direction %+% normalize reflect_direction }
      else None
  | Dielectric { index } ->
      (* The index of air is 1.0, i.e. no refraction. *)
      let index_ratio = if not hit.is_inside_geometry then 1. /. index else index in
      let unit_ray_direction = normalize ray.direction in
      (* The cosine of the angle between the incoming ray and the normal to the hit. *)
      let cos_theta_r = min (unit_ray_direction %.% negate hit.unit_normal) 1. in
      let sin_theta_r = sqrt (1. -. (cos_theta_r *. cos_theta_r)) in

      let rebound_direction =
        (* Where `index ratio * sin(theta) > 1`, Snell's law breaks down and the ray
	   cannot be refracted; it must be reflected instead. *)
        if sin_theta_r *. index_ratio > 1. || reflectance cos_theta_r index_ratio > Random.float 1.0
        then reflect ~v:unit_ray_direction ~n:hit.unit_normal
        else refract ~uv:unit_ray_direction ~n:hit.unit_normal ~index_ratio
      in
      Some { origin = hit.point; direction = rebound_direction }
