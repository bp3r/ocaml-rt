type t = {
  point : Vec3.t;
  unit_normal : Vec3.t;
  time : float;
  is_inside_geometry : bool;
  material : Material.t;
}

let get_sphere (ray : Ray.t) sphere interval =
  let open Vec3 in
  let open Entity.Sphere in
  let ray_to_sphere_vec = center sphere %-% ray.origin in
  let a = ray.direction %.% ray.direction in
  (* `b = -2h` simplifies the below quadratic formula (cancels out the factor of `2`) *)
  let h = ray.direction %.% ray_to_sphere_vec in
  let c = (ray_to_sphere_vec %.% ray_to_sphere_vec) -. (radius sphere *. radius sphere) in
  let discriminant = (h *. h) -. (a *. c) in

  let solution =
    if discriminant >= 0. then
      let first_solution = (h -. sqrt discriminant) /. a in
      if Interval.contains interval first_solution then Some first_solution
      else if discriminant > 0. then
        let second_solution = (h +. sqrt discriminant) /. a in
        if Interval.contains interval second_solution then Some second_solution else None
      else None
    else None
  in

  Option.bind solution (fun time ->
      let point_hit = Ray.at ray time in
      let outward_unit_normal = 1. /. radius sphere *% (point_hit %-% center sphere) in
      let hit_is_inside_geometry = ray.direction %.% outward_unit_normal >= 0. in
      Some
        {
          point = point_hit;
          unit_normal =
            (if hit_is_inside_geometry then negate outward_unit_normal else outward_unit_normal);
          time;
          is_inside_geometry = hit_is_inside_geometry;
          material = material sphere;
        })

let rec get ray entity interval =
  let open Entity in
  let keep_earliest_record prior_record entity' =
    match (get ray entity' interval, prior_record) with
    | Some record, Some prior_record ->
        if record.time < prior_record.time then Some record else Some prior_record
    | Some record, None -> Some record
    | None, Some prior_record -> Some prior_record
    | None, None -> None
  in
  match entity with
  | EList lst -> List.fold_left keep_earliest_record None lst
  | ESphere sphere -> get_sphere ray sphere interval
