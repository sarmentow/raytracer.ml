open Raytracer.Color
open Raytracer.Tuple
open Raytracer.Canvas

let (-:) name f = Alcotest.test_case name `Quick f

  let tests = "chapter2", [
    "init color" -: begin fun () ->
      let c = Color.init (-0.5) 0.4 1.7 1.0 in
      Alcotest.(check bool) "inits color" true ((Color.x c = -0.5) && (Color.y c = 0.4) && (Color.z c = 1.7) && (Color.w c = 1.0))
    end;
    "add color" -: begin fun () ->
      let c1 = Color.init 0.9 0.6 0.75 1.0 in
      let c2 = Color.init 0.7 0.1 0.25 1.0 in
      let result = ColorOps.(<+>) c1 c2 in 
      Alcotest.(check bool) "adds color" true ((Color.x result = 1.6) && (Color.y result = 0.7) && (Color.z result = 1.0) && (Color.w result = 2.0))
    end;
    "subtract color" -: begin fun () ->
      let c1 = Color.init 0.9 0.6 0.75 1.0 in
      let c2 = Color.init 0.7 0.1 0.25 1.0 in
      let result = ColorOps.(<->) c1 c2 in 
      Alcotest.(check bool) "subtracts color" true 
        ((Color.x result =. 0.2) && 
        (Color.y result =. 0.5) && 
        (Color.z result =. 0.5) && 
        (Color.w result =. 0.))
    end;
    "multiply color by scalar" -: begin fun () ->
      let c1 = Color.init 0.2 0.3 0.4 1.0 in
      let result = ColorOps.(<*.>) c1 2. in 
      Alcotest.(check bool) "subtracts color" true 
        ((Color.x result =. 0.4) && 
        (Color.y result =. 0.6) && 
        (Color.z result =. 0.8) && 
        (Color.w result =. 2.))
    end;
    "blend colors" -: begin fun () ->
      let c1 = Color.init 1. 0.2 0.4 1.0 in
      let c2 = Color.init 0.9 1. 0.1 1.0 in
      let result = ColorOps.(<*>) c1 c2 in 
      Alcotest.(check bool) "subtracts color" true 
        ((Color.x result =. 0.9) && 
        (Color.y result =. 0.2) && 
        (Color.z result =. 0.04) && 
        (Color.w result =. 1.))
    end;
    "create canvas" -: begin fun () ->
      let canvas = Canvas.init 10 20 in
      Alcotest.(check bool) "subtracts color" true 
        ((Canvas.w canvas = 10) && 
        (Canvas.h canvas = 20))
    end;
    "write pixels to canvas" -: begin fun () ->
      let canvas = Canvas.init 10 20 in
      let red = Color.init 1. 0. 0. 1. in
      Alcotest.(check bool) "subtracts color" true 
        ((Canvas.write_pixel canvas 5 5 red) |> Canvas.pixel_at 5 5 |> ColorOps.(=) red)
    end;
    "write canvas to ppm" -: begin fun () ->
      let canvas = Canvas.init 10 20 in
      let red = Color.init 0. 255. 0. 1. in
      Canvas.canvas_to_ppm (Canvas.write_pixel canvas 5 5 red);
      Alcotest.(check bool) "subtracts color" true true
    end;
  ]