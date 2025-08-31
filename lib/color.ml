type t = float * float * float

let clamp x ~minimum ~maximum = max (min x maximum) minimum

let make r g b =
  let clamp = clamp ~minimum:0.0 ~maximum:1.0 in
  (clamp r, clamp g, clamp b)

let black = (0., 0., 0.)

let ( %*% ) (x, y, z) (r, g, b) = make (x *. r) (y *. g) (z *. b)

let ( %+% ) (r1, g1, b1) (r2, g2, b2) = (r1 +. r2, g1 +. g2, b1 +. b2)

let ( *% ) f (r, g, b) = (f *. r, f *. g, f *. b)

let write_ppm (r, g, b) out_chan =
  (* PPM usually encodes images transformed to the gamma space as a series of byte-values.
     Taking the square root of values will appropriately transform them. *)
  let format_channel_value x = int_of_float (256. *. clamp ~minimum:0.0 ~maximum:0.999 (sqrt x)) in
  let line =
    Printf.sprintf "%d %d %d\n" (format_channel_value r) (format_channel_value g)
      (format_channel_value b)
  in
  Out_channel.output_string out_chan line
