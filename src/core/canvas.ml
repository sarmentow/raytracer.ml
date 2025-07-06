module Canvas = struct
  type t = { w : int; h : int; pixels : Color.Color.t array array }

  let w t = t.w
  let h t = t.h

  let init w h =
    { w; h; pixels = Array.make_matrix h w (Color.Color.init 0. 0. 0. 1.) }

  let init_with_color w h c = { w; h; pixels = Array.make_matrix h w c }
  let write_pixel c x y color = c.pixels.(y).(x) <- color
  let pixel_at x y c = c.pixels.(y).(x)
  let scale_color c = if c < 0. then 0. else if c > 1. then 255. else c *. 255.

  let color_to_spaced_rgb c =
    Printf.sprintf "%d %d %d"
      (int_of_float (scale_color (Color.Color.x c)))
      (int_of_float (scale_color (Color.Color.y c)))
      (int_of_float (scale_color (Color.Color.z c)))

  let canvas_to_ppm c =
    let b = Buffer.create 1024 in
    Printf.bprintf b "P3\n";
    Printf.bprintf b "%d %d\n" c.w c.h;
    Printf.bprintf b "255\n";
    for y = 0 to c.h - 1 do
      let b_row = Buffer.create 1024 in
      for x = 0 to c.w - 1 do
        let next_color = color_to_spaced_rgb c.pixels.(y).(x) in
        if String.length next_color + Buffer.length b_row >= 70 then (
          Buffer.add_char b_row '\n';
          Buffer.add_buffer b b_row;
          Buffer.clear b_row);
        Buffer.add_string b_row next_color;
        if x = c.w - 1 then Buffer.add_char b_row '\n'
        else Buffer.add_char b_row ' '
      done;
      Buffer.add_buffer b b_row
    done;
    Buffer.add_char b '\n';
    b
end
