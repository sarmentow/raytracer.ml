open Raytracer
open Raytracer.Matrix

let () =
  let size = 100 in
  let c = Canvas.init size size in
  let white = Color.init 1. 1. 1. 1. in
  let radius = 40. in
  let center = float_of_int size /. 2. in

  for hour = 0 to 11 do
    let angle = float_of_int hour *. (Float.pi /. 6.) in
    let point = Tuple.init_point 0. 1. 0. in
    let transform =
      Transformations.rotation_z angle
      >> Transformations.scale radius radius 0.
      >> Transformations.translation center center 0.
    in

    let p = Matrix.mult transform point in
    Canvas.write_pixel
      (int_of_float (Tuple.x p))
      (int_of_float (Tuple.y p))
      white c
  done;

  let oc = open_out "clock.ppm" in
  let b = Canvas.canvas_to_ppm c |> Buffer.to_bytes in
  output_bytes oc b;
  close_out oc
