exception Bad_config of string

(** Don't mandate a user specifies a float in the JSON; allow an int or a float. *)
let parse_float = function
  | `Int i -> float_of_int @@ Yojson.Basic.Util.to_int (`Int i)
  | o -> Yojson.Basic.Util.to_float o

let parse_vec3 jlist =
  let open Yojson.Basic.Util in
  let nlist = convert_each parse_float jlist in
  assert (List.length nlist = 3);
  (List.nth nlist 0, List.nth nlist 1, List.nth nlist 2)

module Position_endpoints = struct
  type t = { start : Vec3.t; finish : Vec3.t }

  let parse alist =
    let open Yojson.Basic.Util in
    { start = parse_vec3 @@ member "start" alist; finish = parse_vec3 @@ member "finish" alist }
end

module type Position_endpoints = module type of Position_endpoints

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

let parse_materials alist =
  let open Yojson.Basic.Util in
  let parse_material name obj : Material.t =
    match obj |> member "type" |> to_string |> String.lowercase_ascii with
    | "dielectric" -> Dielectric { index = obj |> member "index" |> parse_float }
    | "metal" ->
        Metal
          {
            albedo = obj |> member "albedo" |> parse_vec3;
            fuzziness = obj |> member "fuzziness" |> parse_float;
          }
    | "lambertian" -> Lambertian { albedo = obj |> member "albedo" |> parse_vec3 }
    | type' -> raise @@ Bad_config (Printf.sprintf "unrecognized material type: %s" type')
  in
  List.map (fun (name, obj) -> (name, parse_material name obj)) (to_assoc alist)

let parse_world parsed_materials world_arr : Entity.t =
  let open Yojson.Basic.Util in
  let parse_entity obj : Entity.t =
    let material =
      let k = obj |> member "material" |> to_string |> String.lowercase_ascii in
      match List.assoc_opt k parsed_materials with
      | Some mat -> mat
      | None -> raise @@ Bad_config (Printf.sprintf "unrecognized material key: %s" k)
    in

    match obj |> member "geometry" |> to_string |> String.lowercase_ascii with
    | "sphere" ->
        ESphere
          (Entity.Sphere.make
             (obj |> member "center" |> parse_vec3)
             (obj |> member "radius" |> parse_float)
             material)
    | geometry -> raise @@ Bad_config (Printf.sprintf "unrecognized geometry type: %s" geometry)
  in
  EList (convert_each parse_entity world_arr)

let parse_top_level alist =
  let open Yojson.Basic.Util in
  let config_keys = keys alist in
  let required_keys =
    [
      "filename_prefix";
      "n_frames";
      "fps";
      "fov_degrees";
      "image_width";
      "aspect_ratio";
      "thread_count";
      "samples_per_pixel";
      "max_ray_rebounds";
      "camera";
      "focus";
      "materials";
      "world";
    ]
  in

  if not @@ List.for_all (fun k -> List.mem k config_keys) required_keys then
    raise @@ Bad_config "missing required key in JSON top level"
  else
    let filename_prefix = alist |> member "filename_prefix" |> to_string in
    let n_frames = alist |> member "n_frames" |> to_int in
    let fps = alist |> member "fps" |> to_int in
    let fov_degrees = alist |> member "fov_degrees" |> parse_float in
    let image_width = alist |> member "image_width" |> to_int in
    let aspect_ratio = alist |> member "aspect_ratio" |> parse_float in
    let thread_count = alist |> member "thread_count" |> to_int in
    let samples_per_pixel = alist |> member "samples_per_pixel" |> to_int in
    let max_ray_rebounds = alist |> member "max_ray_rebounds" |> to_int in
    let camera = alist |> member "camera" |> Position_endpoints.parse in
    let focus = alist |> member "focus" |> Position_endpoints.parse in
    let materials = alist |> member "materials" |> parse_materials in
    let world = parse_world materials (member "world" alist) in

    {
      filename_prefix;
      n_frames;
      fps;
      fov_degrees;
      image_width;
      aspect_ratio;
      thread_count;
      samples_per_pixel;
      max_ray_rebounds;
      camera;
      focus;
      world;
    }

let parse config_file =
  match Yojson_five.Basic.from_file config_file with
  | Ok (`Assoc alist) -> parse_top_level (`Assoc alist)
  | Ok _ -> raise @@ Bad_config "top level of config must be a JSON object"
  | Error e -> raise @@ Bad_config e
