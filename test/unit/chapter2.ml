open Raytracer.Common.Common
open Raytracer.Color
open Raytracer.Canvas

let ( -: ) name f = Alcotest.test_case name `Quick f

let tests =
  ( "chapter2",
    [
      ( "init color" -: fun () ->
        let c = Color.init (-0.5) 0.4 1.7 1.0 in
        Alcotest.(check bool)
          "inits color" true
          (Color.x c = -0.5
          && Color.y c = 0.4
          && Color.z c = 1.7
          && Color.w c = 1.0) );
      ( "add color" -: fun () ->
        let c1 = Color.init 0.9 0.6 0.75 1.0 in
        let c2 = Color.init 0.7 0.1 0.25 1.0 in
        let result = ColorOps.( <+> ) c1 c2 in
        Alcotest.(check bool)
          "adds color" true
          (Color.x result = 1.6
          && Color.y result = 0.7
          && Color.z result = 1.0
          && Color.w result = 2.0) );
      ( "subtract color" -: fun () ->
        let c1 = Color.init 0.9 0.6 0.75 1.0 in
        let c2 = Color.init 0.7 0.1 0.25 1.0 in
        let result = ColorOps.( <-> ) c1 c2 in
        Alcotest.(check bool)
          "subtracts color" true
          (Color.x result =. 0.2
          && Color.y result =. 0.5
          && Color.z result =. 0.5
          && Color.w result =. 0.) );
      ( "multiply color by scalar" -: fun () ->
        let c1 = Color.init 0.2 0.3 0.4 1.0 in
        let result = ColorOps.( <*.> ) c1 2. in
        Alcotest.(check bool)
          "subtracts color" true
          (Color.x result =. 0.4
          && Color.y result =. 0.6
          && Color.z result =. 0.8
          && Color.w result =. 2.) );
      ( "hadamard product" -: fun () ->
        let c1 = Color.init 1. 0.2 0.4 1.0 in
        let c2 = Color.init 0.9 1. 0.1 1.0 in
        let result = ColorOps.( <*> ) c1 c2 in
        Alcotest.(check bool)
          "hadamard product" true
          (Color.x result =. 0.9
          && Color.y result =. 0.2
          && Color.z result =. 0.04
          && Color.w result =. 1.) );
      ( "create canvas" -: fun () ->
        let canvas = Canvas.init 10 20 in
        Alcotest.(check bool)
          "canvas has proper dimensions and all pixels are black initially" true
          (Canvas.w canvas = 10
          && Canvas.h canvas = 20
          && Array.for_all
               (fun row ->
                 Array.for_all
                   (fun col ->
                     Color.x col = 0.
                     && Color.y col = 0.
                     && Color.z col = 0.
                     && Color.w col = 1.)
                   row)
               canvas.pixels) );
      ( "write pixels to canvas" -: fun () ->
        let canvas = Canvas.init 10 20 in
        let red = Color.init 1. 0. 0. 1. in
        Alcotest.(check bool)
          "red pixel at (5, 5)" true
          (Canvas.write_pixel canvas 5 5 red;
           Canvas.pixel_at 5 5 canvas |> ColorOps.( = ) red) );
      ( "write canvas to ppm" -: fun () ->
        let canvas = Canvas.init 5 3 in
        let ppm = Canvas.canvas_to_ppm canvas in
        Alcotest.(check bool)
          "outputs expected ppm header" true
          (String.starts_with (Buffer.contents ppm) ~prefix:"P3\n5 3\n255") );
      ( "write pixels to canvas and scales to ppm correct values" -: fun () ->
        let canvas = Canvas.init 5 3 in
        let c1 = Color.init 1.5 0. 0. 1. in
        let c2 = Color.init 0. 0.5 0. 1. in
        let c3 = Color.init (-0.5) 0. 1. 1. in
        Canvas.write_pixel canvas 0 0 c1;
        Canvas.write_pixel canvas 2 1 c2;
        Canvas.write_pixel canvas 4 2 c3;
        let ppm = Canvas.canvas_to_ppm canvas in
        let b = Buffer.contents ppm in
        let lines = String.split_on_char '\n' b in
        Alcotest.(check bool)
          "outputs expected ppm file" true
          (match lines with
          | _ :: _ :: _ :: line3 :: line4 :: line5 :: _ ->
              line3 = "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              && line4 = "0 0 0 0 0 0 0 127 0 0 0 0 0 0 0"
              && line5 = "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"
          | _ -> false) );
      ( "write pixels to ppm and breaks lines  to be below 70 chars" -: fun () ->
        let canvas = Canvas.init_with_color 10 2 (Color.init 1. 1. 1. 1.) in
        let ppm = Canvas.canvas_to_ppm canvas in
        let b = Buffer.contents ppm in
        let lines = String.split_on_char '\n' b in
        Alcotest.(check bool)
          "outputs expected ppm file" true
          (List.for_all (fun line -> String.length line <= 70) lines) );
      ( "ppm files are terminated by a newline" -: fun () ->
        let canvas = Canvas.init_with_color 10 2 (Color.init 1. 1. 1. 1.) in
        let ppm = Canvas.canvas_to_ppm canvas in
        let b = Buffer.contents ppm in
        let lines = String.split_on_char '\n' b in
        (* Printf.printf "%s" b; *)
        Alcotest.(check bool)
          "ends in newline" true
          (match List.rev lines with l :: _ -> l = "" | _ -> false) );
    ] )
