open Rt

let () =
  if not (Array.length Sys.argv = 2) then Printf.eprintf "%s <path to config>\n" Sys.argv.(0)
  else exit @@ Render.from_config @@ Config.parse Sys.argv.(1)
