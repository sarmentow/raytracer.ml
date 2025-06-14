module Canvas = struct
  type t = {w: int; h: int; pixels: Color.Color.t array array}
  let w t = t.w
  let h t = t.h
  let init w h = {w; h; pixels = (Array.make_matrix h w (Color.Color.init 0. 0. 0. 1.))}
  let write_pixel c x y color = 
    c.pixels.(y).(x) <- color;
    c
  let pixel_at x y c = c.pixels.(y).(x)
  let canvas_to_ppm c =
    let file = "out.ppm" in
    let oc = open_out file in
    Printf.fprintf oc "P3\n";
    Printf.fprintf oc "%d %d\n" c.w c.h;
    Printf.fprintf oc "255\n";
    for y = 0 to c.h - 1 do
      for x  = 0 to c.w - 1 do
        Printf.fprintf oc "%d %d %d " 
        (int_of_float (Color.Color.x c.pixels.(y).(x)))
        (int_of_float (Color.Color.y c.pixels.(y).(x)))
        (int_of_float (Color.Color.z c.pixels.(y).(x)));
      done;
      Printf.fprintf oc "\n"
    done;
  end