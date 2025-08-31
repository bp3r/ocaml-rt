type t = { origin : Vec3.t; direction : Vec3.t }

let at ray time =
  let open Vec3 in
  ray.origin %+% (time *% ray.direction)

let sample ~point ~span:(sx, sy, sz) ~camera_loc =
  let open Vec3 in
  let random e = (Random.float 1.0 *. e) -. (0.5 *. e) in
  let offset = (random sx, random sy, random sz) in
  { origin = camera_loc; direction = point %+% offset %-% camera_loc }
