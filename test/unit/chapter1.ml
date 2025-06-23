open Raytracer.Common.Common
open Raytracer.Tuple.TupleOperations
open Raytracer.Tuple.Tuple

let (-:) name f = Alcotest.test_case name `Quick f

let p = init 4.3 (-4.2) 3.1 1.0

let magnitude_test v mag = Printf.sprintf "magnitude of vector (%.3f, %.3f, %.3f) is %f" (x v) (y v) (z v) mag -: begin fun() ->
    
    let a1 = v in
    magnitude a1
    |> Alcotest.(check (float 0.001)) (Printf.sprintf "magnitude is %f" mag) mag
end

let normalize_test v v_norm = Printf.sprintf "norm of tuple (%.3f, %.3f, %.3f, %.3f) is (%.3f, %.3f, %.3f, %.3f)" (x v) (y v) (z v) (w v) (x v_norm) (y v_norm) (z v_norm) (w v_norm) -: begin fun() ->
  
    let a1 = v in
    normalize a1 = v_norm
    |> Alcotest.(check bool) "equal to norm" true
end

  let tests = "chapter1", [
  "x" -: begin fun () ->
    x p
    |> Alcotest.(check (float 0.001)) "x" 4.3
  end;

  "y" -: begin fun () ->
    y p
    |> Alcotest.(check (float 0.001)) "y" (-4.2)
  end;

  "z" -: begin fun () ->
    z p
    |> Alcotest.(check (float 0.001)) "z" 3.1
  end;

  "is a point" -: begin fun () ->
    is_point p
    |> Alcotest.(check bool) "is point" true
  end;

  "is not a vector" -: begin fun () ->
    is_vector p
    |> Alcotest.(check bool) "is vector" false
  end;

  "init_point creates vector with w=1." -: begin fun () ->
    (init_point 1. 2. 3.)
    |> (=) (init 1. 2. 3. 1.)
    |> Alcotest.(check bool) "inits point" true
  end;

  "init_vector creates vector with w=0." -: begin fun () ->
    (init_vector 1. 2. 3.)
    |> (=) (init 1. 2. 3. 0.)
    |> Alcotest.(check bool) "inits vector" true
  end;

  "adding two tuples" -: begin fun () ->
    let a1 = init_point 3. (-2.) 5. in 
    let a2 = init_vector (-2.) 3. 1. in
    let a3 = init 1. 1. 6. 1. in
    ((<+>)) a1 a2 = a3
    |> Alcotest.(check bool) "adds tuples" true
  end;

  "subtracting two points" -: begin fun () ->
    let a1 = init_point 3. 2. 1. in 
    let a2 = init_point 5. 6. 7. in
    let a3 = init_vector (-2.) (-4.) (-6.) in
    (<->) a1 a2 = a3
    |> Alcotest.(check bool) "subtracts points" true
  end;

  "subtracting a vector from a point" -: begin fun () ->
    let a1 = init_point 3. 2. 1. in 
    let a2 = init_vector 5. 6. 7. in
    let a3 = init_point (-2.) (-4.) (-6.) in
    (<->) a1 a2 = a3
    |> Alcotest.(check bool) "subtracts vector from point" true
  end;

  "subtracting vectors" -: begin fun () ->
    let a1 = init_vector 3. 2. 1. in 
    let a2 = init_vector 5. 6. 7. in
    let a3 = init_vector (-2.) (-4.) (-6.) in
    (<->) a1 a2 = a3
    |> Alcotest.(check bool) "subtracts vectors" true
  end;

  "negating a vector" -: begin fun () ->
    let a1 = init_vector 1. (-2.) 3. in
    let a2 = init_vector (-1.) 2. (-3.) in
    ((~<->)a1) = a2
    |> Alcotest.(check bool) "negates vector" true
  end;


  "multiplying a tuple by a scalar" -: begin fun () ->
    let a1 = init 1. (-2.) 3. (-4.) in
    let a2 = init 3.5 (-7.) 10.5 (-14.) in
    (<*.>) a1 3.5 = a2
    |> Alcotest.(check bool) "multiplies by scalar" true
  end;

  "multiplying a tuple by a fraction" -: begin fun () ->
    let a1 = init 3.5 (-7.) 10.5 (-14.) in
    let a2 = init 1. (-2.) 3. (-4.) in
    (<*.>) a1 (1. /. 3.5) = a2
    |> Alcotest.(check bool) "multiplies by scalar" true
  end;

  "dividing a tuple by a scalar" -: begin fun () ->
    let a1 = init 1. (-2.) 3. (-4.) in
    let a2 = init 0.5 (-1.) 1.5 (-2.) in
    a1 </.> 2. = a2
    |> Alcotest.(check bool) "divides by scalar" true
  end;

  magnitude_test (init_vector 1. 0. 0.) 1.;
  magnitude_test (init_vector 0. 1. 0.) 1.;
  magnitude_test (init_vector 0. 0. 1.) 1.;
  magnitude_test (init_vector 1. 2. 3.) (sqrt 14.);
  magnitude_test (init_vector (-1.) (-2.) (-3.)) (sqrt 14.);

  normalize_test (init_vector 4. 0. 0.) (init_vector 1. 0. 0.);
  normalize_test (init_vector 1. 2. 3.) (init_vector  (1./.sqrt 14.) (2./.sqrt 14.) (3./.sqrt 14.));
  magnitude_test (normalize (init_vector 1. 2. 3.)) 1.;

  "dot product of 2 tuples" -: begin fun () ->
    let a1 = init_vector 1. 2. 3. in
    let a2 = init_vector 2. 3. 4. in
    (dot a1 a2) =. 20.
    |> Alcotest.(check bool) "dot products" true
  end;

  "cross product of 2 vectors" -: begin fun () ->
    let a1 = init_vector 1. 2. 3. in
    let a2 = init_vector 2. 3. 4. in
    let result = init_vector (-1.) 2. (-1.) in
    ((cross a1 a2 = result) && (cross a2 a1 = (~<->result)))
    |> Alcotest.(check bool) "cross products" true
  end;
]