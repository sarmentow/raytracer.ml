open Raytracer

let () =
  let ray_origin = Tuple.init_point 0. 0. (-5.) in
  let wall_z = 10. in
  let wall_size = 7. in
  let canvas_pixels = 100 in
  let pixel_size = wall_size /. float_of_int canvas_pixels in
  let half = wall_size /. 2. in
  let canvas = Canvas.init canvas_pixels canvas_pixels in
  let color = Color.init 1. 0. 0. 1. in
  let shape = Sphere.init 0 in
  for y = 0 to canvas_pixels - 1 do
    let world_y = half -. (pixel_size *. float_of_int y) in
    for x = 0 to canvas_pixels - 1 do
      let world_x = -.half +. (pixel_size *. float_of_int x) in
      let position = Tuple.init_point world_x world_y wall_z in
      let r =
        Ray.init ray_origin
          (Tuple.normalize (Tuple.( <-> ) position ray_origin))
      in
      let xs = Ray.intersect shape r in
      let hit_instance = Intersection.hit xs in
      if hit_instance != None then Canvas.write_pixel x y color canvas
    done
  done;

  let oc = open_out "sphere.ppm" in
  let b = Canvas.canvas_to_ppm canvas |> Buffer.to_bytes in
  output_bytes oc b;
  close_out oc
