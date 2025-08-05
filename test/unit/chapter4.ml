open Raytracer.Transformations
open Raytracer.Matrix
open Raytracer.Tuple

let ( -: ) name f = Alcotest.test_case name `Quick f

let matrix_testable =
  let pp fmt (m : Mat.t) =
    Format.fprintf fmt "@[<v>Matrix %dx%d:@," m.rows m.cols;
    for i = 0 to m.rows - 1 do
      Format.fprintf fmt "@[<h>";
      for j = 0 to m.cols - 1 do
        Format.fprintf fmt "%8.3f" (Mat.get m i j);
        if j < m.cols - 1 then Format.fprintf fmt " "
      done;
      Format.fprintf fmt "@]";
      if i < m.rows - 1 then Format.fprintf fmt "@,"
    done;
    Format.fprintf fmt "@]"
  in
  let equal m1 m2 = Mat.compare_matrices m1 m2 in
  Alcotest.testable pp equal

let tests =
  ( "chapter4",
    [
      (* 
        Scenario: Multiplying by a translation matrix
        Given transform ← translation(5, -3, 2)
        And p ← point(-3, 4, 5)
        Then transform * p = point(2, 1, 7)
      *)
      ( "Multiplying by a translation matrix" -: fun () ->
        let transform = Transform.translation 5. (-3.) 2. in
        let point = TupleOperations.init_point (-3.) 4. 5. in
        let expected = TupleOperations.init_point 2. 1. 7. in
        let product = Mat.mult transform point in

        Alcotest.(check matrix_testable)
          "transform * p = point(2, 1, 7)" expected product );
      (* 
        Scenario: Multiplying by the inverse of a translation matrix
        Given transform ← translation(5, -3, 2)
        And inv ← inverse(transform)
        And p ← point(-3, 4, 5)
        Then inv * p = point(-8, 7, 3)
      *)
      ( "Multiplying by the inverse of a translation matrix" -: fun () ->
        let transform = Transform.translation 5. (-3.) 2. in
        let inv = Mat.inverse transform in
        let point = TupleOperations.init_point (-3.) 4. 5. in
        let expected = TupleOperations.init_point (-8.) 7. 3. in
        let product = Mat.mult inv point in

        Alcotest.(check matrix_testable)
          "inv * p = point(-8, 7, 3)" expected product );
      (* 
        Scenario: Translation does not affect vectors
        Given transform ← translation(5, -3, 2)
        And v ← vector(-3, 4, 5)
        Then transform * v = v
      *)
      ( "Translation does not affect vectors" -: fun () ->
        let transform = Transform.translation 5. (-3.) 2. in
        let vector = TupleOperations.init_vector (-3.) 4. 5. in
        let expected = vector in
        let product = Mat.mult transform vector in

        Alcotest.(check matrix_testable) "transform * v = v" expected product );
      (* 
        Scenario: A scaling matrix applied to a point
        Given transform ← scaling(2, 3, 4)
        And p ← point(-4, 6, 8)
        Then transform * p = point(-8, 18, 32)
      *)
      ( "A scaling matrix applied to a point" -: fun () ->
        let transform = Transform.scale 2. 3. 4. in
        let point = TupleOperations.init_point (-4.) 6. 8. in
        let expected = TupleOperations.init_point (-8.) 18. 32. in
        let product = Mat.mult transform point in

        Alcotest.(check matrix_testable)
          "transform * p = point(-8, 18, 32)" expected product );
      (* 
        Scenario: A scaling matrix applied to a vector
        Given transform ← scaling(2, 3, 4)
        And v ← vector(-4, 6, 8)
        Then transform * v = vector(-8, 18, 32)
      *)
      ( "A scaling matrix applied to a vector" -: fun () ->
        let transform = Transform.scale 2. 3. 4. in
        let vector = TupleOperations.init_vector (-4.) 6. 8. in
        let expected = TupleOperations.init_vector (-8.) 18. 32. in
        let product = Mat.mult transform vector in

        Alcotest.(check matrix_testable)
          "transform * v = vector(-8, 18, 32)" expected product );
      (* 
        Scenario: Multiplying by the inverse of a scaling matrix
        Given transform ← scaling(2, 3, 4)
        And inv ← inverse(transform)
        And v ← vector(-4, 6, 8)
        Then inv * v = vector(-2, 2, 2)
      *)
      ( "Multiplying by the inverse of a scaling matrix" -: fun () ->
        let transform = Transform.scale 2. 3. 4. in
        let inv = Mat.inverse transform in
        let vector = TupleOperations.init_vector (-4.) 6. 8. in
        let expected = TupleOperations.init_vector (-2.) 2. 2. in
        let product = Mat.mult inv vector in

        Alcotest.(check matrix_testable)
          "inv * v = vector(-2, 2, 2)" expected product );
      (* 
        Scenario: Reflection is scaling by a negative value
        Given transform ← scaling(-1, 1, 1)
        And p ← point(2, 3, 4)
        Then transform * p = point(-2, 3, 4)
      *)
      ( "Reflection is scaling by a negative value" -: fun () ->
        let transform = Transform.scale (-1.) 1. 1. in
        let point = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point (-2.) 3. 4. in
        let product = Mat.mult transform point in

        Alcotest.(check matrix_testable)
          "transform * p = point(-2, 3, 4)" expected product );
      (* 
        Scenario: Rotating a point around the x axis
        Given p ← point(0, 1, 0)
        And half_quarter← rotation_x(π / 4)
        And full_quarter← rotation_x(π / 2)
        Then half_quarter * p = point(0, √2/2, √2/2)
        And full_quarter * p = point(0, 0, 1)
      *)
      ( "Rotating a point around the x axis" -: fun () ->
        let p = TupleOperations.init_point 0. 1. 0. in
        let half_quarter = Transform.rotation_x (Float.pi /. 4.) in
        let full_quarter = Transform.rotation_x (Float.pi /. 2.) in
        let sqrt2_2 = sqrt 2. /. 2. in
        let expected_half = TupleOperations.init_point 0. sqrt2_2 sqrt2_2 in
        let expected_full = TupleOperations.init_point 0. 0. 1. in
        let product_half = Mat.mult half_quarter p in
        let product_full = Mat.mult full_quarter p in

        Alcotest.(check matrix_testable)
          "half_quarter * p = point(0, √2/2, √2/2)" expected_half product_half;
        Alcotest.(check matrix_testable)
          "full_quarter * p = point(0, 0, 1)" expected_full product_full );
      (* 
        Scenario: Rotating a point around the y axis
        Given p ← point(0, 0, 1)
        And half_quarter← rotation_y(π / 4)
        And full_quarter← rotation_y(π / 2)
        Then half_quarter * p = point(√2/2, 0, √2/2)
        And full_quarter * p = point(1, 0, 0)
      *)
      ( "Rotating a point around the y axis" -: fun () ->
        let p = TupleOperations.init_point 0. 0. 1. in
        let half_quarter = Transform.rotation_y (Float.pi /. 4.) in
        let full_quarter = Transform.rotation_y (Float.pi /. 2.) in
        let sqrt2_2 = sqrt 2. /. 2. in
        let expected_half = TupleOperations.init_point sqrt2_2 0. sqrt2_2 in
        let expected_full = TupleOperations.init_point 1. 0. 0. in
        let product_half = Mat.mult half_quarter p in
        let product_full = Mat.mult full_quarter p in

        Alcotest.(check matrix_testable)
          "half_quarter * p = point(√2/2, 0, √2/2)" expected_half product_half;
        Alcotest.(check matrix_testable)
          "full_quarter * p = point(1, 0, 0)" expected_full product_full );
      (* 
        Scenario: Rotating a point around the z axis
        Given p ← point(0, 1, 0)
        And half_quarter← rotation_z(π / 4)
        And full_quarter← rotation_z(π / 2)
        Then half_quarter * p = point(-√2/2, √2/2, 0)
        And full_quarter * p = point(-1, 0, 0)
      *)
      ( "Rotating a point around the z axis" -: fun () ->
        let p = TupleOperations.init_point 0. 1. 0. in
        let half_quarter = Transform.rotation_z (Float.pi /. 4.) in
        let full_quarter = Transform.rotation_z (Float.pi /. 2.) in
        let sqrt2_2 = sqrt 2. /. 2. in
        let expected_half = TupleOperations.init_point (-.sqrt2_2) sqrt2_2 0. in
        let expected_full = TupleOperations.init_point (-1.) 0. 0. in
        let product_half = Mat.mult half_quarter p in
        let product_full = Mat.mult full_quarter p in

        Alcotest.(check matrix_testable)
          "half_quarter * p = point(-√2/2, √2/2, 0)" expected_half product_half;
        Alcotest.(check matrix_testable)
          "full_quarter * p = point(-1, 0, 0)" expected_full product_full );
      (* 
        Scenario: A shearing transformation moves x in proportion to y
        Given transform ← shearing(1, 0, 0, 0, 0, 0)
        And p ← point(2, 3, 4)
        Then transform * p = point(5, 3, 4)
      *)
      ( "A shearing transformation moves x in proportion to y" -: fun () ->
        let transform = Transform.shearing 1. 0. 0. 0. 0. 0. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 5. 3. 4. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(5, 3, 4)" expected product );
      (* 
        Scenario: A shearing transformation moves x in proportion to z
        Given transform ← shearing(0, 1, 0, 0, 0, 0)
        And p ← point(2, 3, 4)
        Then transform * p = point(6, 3, 4)
      *)
      ( "A shearing transformation moves x in proportion to z" -: fun () ->
        let transform = Transform.shearing 0. 1. 0. 0. 0. 0. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 6. 3. 4. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(6, 3, 4)" expected product );
      (* 
        Scenario: A shearing transformation moves y in proportion to x
        Given transform ← shearing(0, 0, 1, 0, 0, 0)
        And p ← point(2, 3, 4)
        Then transform * p = point(2, 5, 4)
      *)
      ( "A shearing transformation moves y in proportion to x" -: fun () ->
        let transform = Transform.shearing 0. 0. 1. 0. 0. 0. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 2. 5. 4. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(2, 5, 4)" expected product );
      (* 
        Scenario: A shearing transformation moves y in proportion to z
        Given transform ← shearing(0, 0, 0, 1, 0, 0)
        And p ← point(2, 3, 4)
        Then transform * p = point(2, 7, 4)
      *)
      ( "A shearing transformation moves y in proportion to z" -: fun () ->
        let transform = Transform.shearing 0. 0. 0. 1. 0. 0. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 2. 7. 4. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(2, 7, 4)" expected product );
      (* 
        Scenario: A shearing transformation moves z in proportion to x
        Given transform ← shearing(0, 0, 0, 0, 1, 0)
        And p ← point(2, 3, 4)
        Then transform * p = point(2, 3, 6)
      *)
      ( "A shearing transformation moves z in proportion to x" -: fun () ->
        let transform = Transform.shearing 0. 0. 0. 0. 1. 0. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 2. 3. 6. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(2, 3, 6)" expected product );
      (* 
        Scenario: A shearing transformation moves z in proportion to y
        Given transform ← shearing(0, 0, 0, 0, 0, 1)
        And p ← point(2, 3, 4)
        Then transform * p = point(2, 3, 7)
      *)
      ( "A shearing transformation moves z in proportion to y" -: fun () ->
        let transform = Transform.shearing 0. 0. 0. 0. 0. 1. in
        let p = TupleOperations.init_point 2. 3. 4. in
        let expected = TupleOperations.init_point 2. 3. 7. in
        let product = Mat.mult transform p in

        Alcotest.(check matrix_testable)
          "transform * p = point(2, 3, 7)" expected product );
      (* 
        Scenario: Individual transformations are applied in sequence
        Given p ← point(1, 0, 1)
        And A ← rotation_x(π / 2)
        And B ← scaling(5, 5, 5)
        And C ← translation(10, 5, 7)
        # apply rotation first
        When p2 ← A * p
        Then p2 = point(1, -1, 0)
        # then apply scaling
        When p3 ← B * p2
        Then p3 = point(5, -5, 0)
        # then apply translation
        When p4 ← C * p3
        Then p4 = point(15, 0, 7)
      *)
      ( "Individual transformations are applied in sequence" -: fun () ->
        let p = TupleOperations.init_point 1. 0. 1. in
        let a = Transform.rotation_x (Float.pi /. 2.) in
        let b = Transform.scale 5. 5. 5. in
        let c = Transform.translation 10. 5. 7. in

        (* apply rotation first *)
        let p2 = Mat.mult a p in
        let expected_p2 = TupleOperations.init_point 1. (-1.) 0. in
        Alcotest.(check matrix_testable) "p2 = point(1, -1, 0)" expected_p2 p2;

        (* then apply scaling *)
        let p3 = Mat.mult b p2 in
        let expected_p3 = TupleOperations.init_point 5. (-5.) 0. in
        Alcotest.(check matrix_testable) "p3 = point(5, -5, 0)" expected_p3 p3;

        (* then apply translation *)
        let p4 = Mat.mult c p3 in
        let expected_p4 = TupleOperations.init_point 15. 0. 7. in
        Alcotest.(check matrix_testable) "p4 = point(15, 0, 7)" expected_p4 p4
      );
      (* 
        Scenario: Chained transformations must be applied in reverse order
        Given p ← point(1, 0, 1)
        And A ← rotation_x(π / 2)
        And B ← scaling(5, 5, 5)
        And C ← translation(10, 5, 7)
        When T ← C * B * A
        Then T * p = point(15, 0, 7)
      *)
      ( "Chained transformations must be applied in reverse order" -: fun () ->
        let p = TupleOperations.init_point 1. 0. 1. in
        let a = Transform.rotation_x (Float.pi /. 2.) in
        let b = Transform.scale 5. 5. 5. in
        let c = Transform.translation 10. 5. 7. in

        (* T = C * B * A *)
        let t = Mat.mult c (Mat.mult b a) in
        let result = Mat.mult t p in
        let expected = TupleOperations.init_point 15. 0. 7. in

        Alcotest.(check matrix_testable)
          "T * p = point(15, 0, 7)" expected result );
    ] )
