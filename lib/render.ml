type frame_config = {
  thread_count : int;
  filename : string;
  fov_rads : float;
  image_width : int;
  image_height : int;
  camera_loc : Vec3.t;
  focus_loc : Vec3.t;
  samples_per_pixel : int;
  max_ray_rebounds : int;
  world : Entity.t;
}
(** As `Config.t` but contains settings used for only a single frame, not the entire render. *)

(** Colors the background when there is no ray hit. *)
let lerp_fn = Vec3.lerp (1.0, 1.0, 1.0) (0.5, 0.7, 1.0)

(** Renders using an individual ray (including any child rays it spawns). If [ray] does not hit
    [entity] then a [Vec3.lerp_function] is used to color the background instead.*)
let process_ray ray entity lerp_fn max_rebound_count =
  let rec process_ray' ray entity lerp_fn rebounded =
    let open Color in
    if rebounded = max_rebound_count then Color.black
    else
      (* The hit point of a ray with a surface may be slightly off due to roundoff error.
         If this happens, the rebounding ray may near-instantly collide with that surface again, very
         close to its origin. This causes visual artifacts known as 'shadow acne'. To prevent this,
         we ignore hits that are very close to the origin of a ray (i.e. t < 0.001). *)
      match Hit.get ray entity (0.001, Float.max_float) with
      | Some hit -> (
          match Material_ops.scatter ray hit with
          | Some scattered_ray ->
              let scatter_color = process_ray' scattered_ray entity lerp_fn (1 + rebounded) in
              Material_ops.albedo hit.material %*% scatter_color
          | None -> Color.black)
      | None ->
          let _, normalized_y, _ = Vec3.normalize ray.direction in
          let x, y, z = lerp_fn @@ (0.5 *. (normalized_y +. 1.)) in
          Color.make x y z
  in
  process_ray' ray entity lerp_fn 0

(** Renders the [i]th scanline; returns the line as an array vector of [image_width] colors.
    [pixels_start_point] is the location of the first (top left) pixel in space; [pixel_delta] the
    vectors between pixels in the x and y directions. *)
let process_scanline ~i ~pixels_start_point ~pixel_delta_u ~pixel_delta_v ~(config : frame_config) =
  let create_averaged_color j =
    let open Vec3 in
    let point =
      pixels_start_point %+% (float_of_int j *% pixel_delta_u) %+% (float_of_int i *% pixel_delta_v)
    in
    let sample_rays =
      Array.init config.samples_per_pixel (fun _ ->
          Ray.sample ~point ~span:(pixel_delta_u %+% pixel_delta_v) ~camera_loc:config.camera_loc)
    in

    let sample_colors =
      Array.map
        (fun ray -> process_ray ray config.world lerp_fn config.max_ray_rebounds)
        sample_rays
    in
    let open Color in
    1. /. float_of_int config.samples_per_pixel *% Array.fold_left ( %+% ) Color.black sample_colors
  in
  Array.init config.image_width create_averaged_color

let process_frame (config : frame_config) =
  let open Vec3 in
  let focal_length = length (config.camera_loc %-% config.focus_loc) in

  (* Basis vectors. *)
  assert (config.camera_loc <> config.focus_loc);
  let w = normalize (config.camera_loc %-% config.focus_loc) in
  let up = if w <> (0.0, 1.0, 0.0) then (0.0, 1.0, 0.0) else (0.0, 0.0, 1.0) in
  let u = normalize @@ cross up w in
  let v = cross w u in

  (* The viewport is the 3D representation of the rendered image used in calculating ray hits. *)
  let viewport_height =
    let h = tan (config.fov_rads /. 2.) in
    2. *. h *. focal_length
  in

  let fl_image_width = float_of_int config.image_width in
  let fl_image_height = float_of_int config.image_height in
  let viewport_width = viewport_height *. (fl_image_width /. fl_image_height) in

  (* The viewport deltas are vectors to reach the viewport edges. *)
  let viewport_delta_u = viewport_width *% u in
  let viewport_delta_v = viewport_height *% negate v in

  let viewport_top_left =
    config.camera_loc %-% (focal_length *% w) %-% (0.5 *% (viewport_delta_u %+% viewport_delta_v))
  in

  (* The pixel delta contains the x/y distances between individual pixels in the rendered image when
     they are overlaid on the viewport, as vectors. *)
  let pixel_delta_u = 1. /. fl_image_width *% viewport_delta_u in
  let pixel_delta_v = 1. /. fl_image_height *% viewport_delta_v in
  let pixel_delta = pixel_delta_u %+% pixel_delta_v in

  (* In order for pixels to span the viewport with a gap of `pixel delta` between each, they must
     start at an offset of half the delta into the viewport. *)
  let pixels_start_point = viewport_top_left %+% (0.5 *% pixel_delta) in

  print_endline "Rendering...";

  let scanlines =
    let open Domainslib in
    (* Scanlines that are busier (and thus take longer to render) are likely to occur in clusters
     rather than be distributed evenly throughout the image. Chunks to render in parallel are thus
     allocated in a round-robin fashion. *)
    let chunked_idx =
      let open List in
      let initial_chunks =
        rev
          (init config.thread_count (fun i ->
               rev
                 (init (config.image_height / config.thread_count) (fun j ->
                      (j * config.thread_count) + i))))
      in

      let first_chunk = hd initial_chunks in
      let greatest_value = hd first_chunk in
      (* Account for any leftover scanlines that can't be evenly split amongst threads. *)
      let missing_idx = config.image_height - greatest_value - 1 in

      if missing_idx > 0 then
        (init missing_idx (fun i -> config.image_height - 1 - i) @ first_chunk) :: tl initial_chunks
      else initial_chunks
    in

    let result_chan = Chan.make_unbounded () in

    let create_chunk_processor chunk =
     fun () ->
      let scanlines_by_idx =
        List.map
          (fun i ->
            (i, process_scanline ~i ~pixels_start_point ~pixel_delta_u ~pixel_delta_v ~config))
          chunk
      in
      Chan.send result_chan scanlines_by_idx
    in

    let pool = Task.setup_pool ~num_domains:(config.thread_count - 1) () in

    let promises =
      List.map (fun chunk -> Task.async pool (create_chunk_processor chunk)) chunked_idx
    in

    Task.run pool (fun _ -> List.iter (fun promise -> Task.await pool promise) promises);
    Task.teardown_pool pool;

    let scanlines_by_idx =
      let results = List.map (fun _ -> Chan.recv result_chan) promises in
      (* Time-complexity OK here; gathering results accounts for very little of the runtime. *)
      let merged_results = List.fold_right ( @ ) results [] in
      List.sort (fun (i, _) (j, _) -> Int.compare i j) merged_results
    in

    List.map (fun (_, scanline) -> scanline) scanlines_by_idx
  in

  Out_channel.with_open_text config.filename (fun chan ->
      Printf.fprintf chan "P3\n%d %d\n255\n" config.image_width config.image_height;
      List.iter (fun scanline -> Array.iter (fun px -> Color.write_ppm px chan) scanline) scanlines);

  print_endline "Complete!"

let from_config (config : Config.t) =
  if not Sys.unix then failwith "only Unix is supported"
  else if Sys.command "which ffmpeg >/dev/null" <> 0 then
    failwith "`ffmpeg` is required but is not in $PATH"
  else
    let image_height = int_of_float (float_of_int config.image_width /. config.aspect_ratio) in
    assert (image_height > 0);
    let fov_rads = config.fov_degrees *. Float.pi /. 180. in
    let camera_lerp = Vec3.lerp config.camera.start config.camera.finish in
    let focus_lerp = Vec3.lerp config.focus.start config.focus.finish in

    for i = 0 to config.n_frames - 1 do
      Printf.printf "Processing frame %d/%d\n" (i + 1) config.n_frames;
      process_frame
        {
          thread_count = config.thread_count;
          filename = Printf.sprintf "%s_%d.ppm" config.filename_prefix i;
          fov_rads;
          image_width = config.image_width;
          image_height;
          camera_loc = camera_lerp @@ (float_of_int i /. float_of_int (config.n_frames - 1));
          focus_loc = focus_lerp @@ (float_of_int i /. float_of_int (config.n_frames - 1));
          samples_per_pixel = config.samples_per_pixel;
          max_ray_rebounds = config.max_ray_rebounds;
          world = config.world;
        }
    done;

    if config.n_frames > 1 then (
      print_endline "Outputting video...";
      Sys.command
      @@ Printf.sprintf
           "ffmpeg -y -framerate %d -i %s_%%d.ppm -c:v libx264 -vf \
            \"format=yuv420p,pad=ceil(iw/2)*2:ceil(ih/2)*2\" %s.mp4"
           config.fps config.filename_prefix config.filename_prefix)
    else 0
