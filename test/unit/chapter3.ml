open Raytracer.Matrix

let ( -: ) name f = Alcotest.test_case name `Quick f

let matrix_testable =
  let pp fmt (m : Mat.t) = Format.fprintf fmt "Matrix %dx%d" m.rows m.cols in
  let equal m1 m2 = Mat.compare_matrices m1 m2 in
  Alcotest.testable pp equal

let float_array_testable = Alcotest.array (Alcotest.float 0.1)

let tests =
  ( "chapter3",
    [
      ( "correctly transforms a matrix from its string representation"
      -: fun () ->
        let s =
          "  | 1    | 2    | 3    | 4    |\n\
           | 5.5  | 6.5  | 7.5  | 8.5  |\n\
           | 9    | 10   | 11   | 12   |\n\
           | 13.5 | 14.5 | 15.5 | 16.5 |  "
        in
        let arr_mat =
          Mat.mat_of_matrix
            [|
              [| 1.; 2.; 3.; 4. |];
              [| 5.5; 6.5; 7.5; 8.5 |];
              [| 9.; 10.; 11.; 12. |];
              [| 13.5; 14.5; 15.5; 16.5 |];
            |]
        in
        let s_mat = Mat.mat_of_str s in
        Alcotest.(check matrix_testable)
          "Matrix values, rows and cols from string repr should match matrix \
           from array representation"
          s_mat arr_mat );
      ( "Constructing and inspecting a 4x4 matrix" -: fun () ->
        let s =
          "  | 1    | 2    | 3    | 4    |\n\
           | 5.5  | 6.5  | 7.5  | 8.5  |\n\
           | 9    | 10   | 11   | 12   |\n\
           | 13.5 | 14.5 | 15.5 | 16.5 |  "
        in
        let s_mat = Mat.mat_of_str s in
        let arr =
          [|
            [| 1.; 2.; 3.; 4. |];
            [| 5.5; 6.5; 7.5; 8.5 |];
            [| 9.; 10.; 11.; 12. |];
            [| 13.5; 14.5; 15.5; 16.5 |];
          |]
        in
        let float_array_testable = Alcotest.array (Alcotest.float 0.1) in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d
          (arr |> Mat.array_of_matrix) );
      ( "Constructing and inspecting a 2x2 matrix" -: fun () ->
        let s = " | -3 | 5 |\n              | 1 | -2 | " in
        let s_mat = Mat.mat_of_str s in
        let arr = [| [| -3.; 5. |]; [| 1.; -2. |] |] in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d
          (arr |> Mat.array_of_matrix) );
      ( "Constructing and inspecting a 3x3 matrix" -: fun () ->
        let s = " | -3 | 5 | 0  |\n| 1 | -2 | -7 | \n| 0 | 1  | 1  |" in
        let s_mat = Mat.mat_of_str s in
        let arr =
          [| [| -3.; 5.; 0. |]; [| 1.; -2.; -7. |]; [| 0.; 1.; 1. |] |]
        in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d
          (arr |> Mat.array_of_matrix) );
      ( "Comparing matrices" -: fun () ->
        let s =
          "| 1 | 2 | 3 | 4 |\n\
           | 5 | 6 | 7 | 8 |\n\
           | 9 | 8 | 7 | 6 |\n\
           | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s in
        let b = Mat.mat_of_str s in
        Alcotest.(check bool)
          "Matrices from the same string representation should match" true
          (Mat.compare_matrices a b) );
      ( "Comparing matrices" -: fun () ->
        let s1 =
          "| 1 | 2 | 3 | 4 |\n\
           | 5 | 6 | 7 | 8 |\n\
           | 9 | 8 | 7 | 6 |\n\
           | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s1 in
        let s2 =
          "| 2 | 3 | 4 | 5 |\n\
           | 6 | 7 | 8 | 9 |\n\
           | 8 | 7 | 6 | 5 |\n\
           | 4 | 3 | 2 | 1 |"
        in
        let b = Mat.mat_of_str s2 in
        Alcotest.(check bool)
          "Matrices from different string representations should not match"
          false (Mat.compare_matrices a b) );
      ( "Multiplying matrices" -: fun () ->
        let s1 =
          "| 1 | 2 | 3 | 4 |\n\
           | 5 | 6 | 7 | 8 |\n\
           | 9 | 8 | 7 | 6 |\n\
           | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s1 in
        let s2 =
          "| -2 | 1 | 2 | 3 |\n\
           | 3 | 2 | 1 | -1 |\n\
           | 4 | 3 | 6 | 5 |\n\
           | 1 | 2 | 7 | 8 |"
        in
        let b = Mat.mat_of_str s2 in
        let s3 =
          "| 20| 22 | 50 | 48 |\n\
           | 44| 54 | 114 | 108 |\n\
           | 40| 58 | 110 | 102 |\n\
           | 16| 26 | 46 | 42 |"
        in
        let expected_result = Mat.mat_of_str s3 in
        Alcotest.(check matrix_testable)
          "Matrix mult result is not the expected" expected_result
          (Mat.mult a b) );
      ( "Identity matrix properties" -: fun () ->
        let identity_s =
          "|1|0|0|0|\n\
          \                          |0|1|0|0|\n\
          \                          |0|0|1|0|\n\
          \                          |0|0|0|1|"
        in
        let identity = Mat.mat_of_str identity_s in
        let s =
          "| 0 | 1 | 2 | 4 |\n\
          \                 | 1 | 2 | 4 | 8 |\n\
          \                 | 2 | 4 | 8 | 16 |\n\
          \                 | 4 | 8 | 16 | 32 |"
        in
        let mat = Mat.mat_of_str s in
        let result = Mat.mult mat identity in
        Alcotest.(check matrix_testable)
          "Matrix mult by identity should be itself" mat result );
      ( "Transposing a matrix" -: fun () ->
        let s =
          "| 0 | 9 | 3 | 0 |\n\
          \                 | 9 | 8 | 0 | 8 |\n\
          \                 | 1 | 8 | 5 | 3 |\n\
          \                 | 0 | 0 | 5 | 8 |"
        in
        let a = Mat.mat_of_str s in
        let t_a = Mat.transpose a in
        let expected_s =
          "| 0 | 9 | 1 | 0 |\n\
          \                          | 9 | 8 | 8 | 0 |\n\
          \                          | 3 | 0 | 5 | 5 |\n\
          \                          | 0 | 8 | 3 | 8 |"
        in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check matrix_testable)
          "Transposed matrix is incorrect" expected t_a );
      ( "Transposing the identity matrix" -: fun () ->
        let s =
          "|1|0|0|0|\n\
          \                 |0|1|0|0|\n\
          \                 |0|0|1|0|\n\
          \                 |0|0|0|1|"
        in
        let identity = Mat.mat_of_str s in
        let t_identity = Mat.transpose identity in
        Alcotest.(check matrix_testable)
          "Transposed identity doesn't equal identity" identity t_identity );
      (* New tests for matrix operations *)
      ( "A submatrix of a 3x3 matrix is a 2x2 matrix" -: fun () ->
        let s = "| 1 | 5 | 0 |\n| -3 | 2 | 7 |\n| 0 | 6 | -3 |" in
        let a = Mat.mat_of_str s in
        let sub = Mat.submat a 0 2 in
        let expected_s = "| -3 | 2 |\n| 0 | 6 |" in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check matrix_testable)
          "Submatrix should match expected 2x2 matrix" expected sub );
      ( "A submatrix of a 4x4 matrix is a 3x3 matrix" -: fun () ->
        let s =
          "| -6 | 1 | 1 | 6 |\n\
           | -8 | 5 | 8 | 6 |\n\
           | -1 | 0 | 8 | 2 |\n\
           | -7 | 1 | -1 | 1 |"
        in
        let a = Mat.mat_of_str s in
        let sub = Mat.submat a 2 1 in
        let expected_s = "| -6 | 1 | 6 |\n| -8 | 8 | 6 |\n| -7 | -1 | 1 |" in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check matrix_testable)
          "Submatrix should match expected 3x3 matrix" expected sub );
      ( "Calculating a minor of a 3x3 matrix" -: fun () ->
        let s = "| 3 | 5 | 0 |\n| 2 | -1 | -7 |\n| 6 | -1 | 5 |" in
        let a = Mat.mat_of_str s in
        let b = Mat.submat a 1 0 in
        let det_b = Mat.det b in
        let minor_val = Mat.minor a 1 0 in
        Alcotest.(check (Alcotest.float 0.1))
          "Determinant of submatrix should be 25" 25. det_b;
        Alcotest.(check (Alcotest.float 0.1)) "Minor should be 25" 25. minor_val
      );
      ( "Calculating a cofactor of a 3x3 matrix" -: fun () ->
        let s = "| 3 | 5 | 0 |\n| 2 | -1 | -7 |\n| 6 | -1 | 5 |" in
        let a = Mat.mat_of_str s in
        let minor_0_0 = Mat.minor a 0 0 in
        let cofactor_0_0 = Mat.cofactor a 0 0 in
        let minor_1_0 = Mat.minor a 1 0 in
        let cofactor_1_0 = Mat.cofactor a 1 0 in
        Alcotest.(check (Alcotest.float 0.1))
          "Minor(0,0) should be -12" (-12.) minor_0_0;
        Alcotest.(check (Alcotest.float 0.1))
          "Cofactor(0,0) should be -12" (-12.) cofactor_0_0;
        Alcotest.(check (Alcotest.float 0.1))
          "Minor(1,0) should be 25" 25. minor_1_0;
        Alcotest.(check (Alcotest.float 0.1))
          "Cofactor(1,0) should be -25" (-25.) cofactor_1_0 );
      ( "Testing an invertible matrix for invertibility" -: fun () ->
        let s =
          "| 6 | 4 | 4 | 4 |\n\
           | 5 | 5 | 7 | 6 |\n\
           | 4 | -9 | 3 | -7 |\n\
           | 9 | 1 | 7 | -6 |"
        in
        let a = Mat.mat_of_str s in
        let det_a = Mat.det a in
        Alcotest.(check (Alcotest.float 0.1))
          "Determinant should be -2120" (-2120.) det_a;
        (* Test that matrix is invertible by checking determinant is not zero *)
        Alcotest.(check bool)
          "Matrix should be invertible" true
          (abs_float det_a > 0.001) );
      ( "Testing a noninvertible matrix for invertibility" -: fun () ->
        let s =
          "| -4 | 2 | -2 | -3 |\n\
           | 9 | 6 | 2 | 6 |\n\
           | 0 | -5 | 1 | -5 |\n\
           | 0 | 0 | 0 | 0 |"
        in
        let a = Mat.mat_of_str s in
        let det_a = Mat.det a in
        Alcotest.(check (Alcotest.float 0.1)) "Determinant should be 0" 0. det_a;
        (* Test that matrix is not invertible *)
        Alcotest.(check bool)
          "Matrix should not be invertible" false
          (abs_float det_a > 0.001) );
      ( "Calculating the inverse of a matrix" -: fun () ->
        let s =
          "| -5 | 2 | 6 | -8 |\n\
           | 1 | -5 | 1 | 8 |\n\
           | 7 | 7 | -6 | -7 |\n\
           | 1 | -3 | 7 | 4 |"
        in
        let a = Mat.mat_of_str s in
        let b = Mat.inverse a in
        let det_a = Mat.det a in
        let cofactor_2_3 = Mat.cofactor a 2 3 in
        let cofactor_3_2 = Mat.cofactor a 3 2 in
        let expected_s =
          "| 0.21805 | 0.45113 | 0.24060 | -0.04511 |\n\
           | -0.80827 | -1.45677 | -0.44361 | 0.52068 |\n\
           | -0.07895 | -0.22368 | -0.05263 | 0.19737 |\n\
           | -0.52256 | -0.81391 | -0.30075 | 0.30639 |"
        in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check (Alcotest.float 0.1))
          "Determinant should be 532" 532. det_a;
        Alcotest.(check (Alcotest.float 0.1))
          "Cofactor(2,3) should be -160" (-160.) cofactor_2_3;
        Alcotest.(check (Alcotest.float 0.01))
          "B[3,2] should be -160/532" (-160. /. 532.) (Mat.get b 3 2);
        Alcotest.(check (Alcotest.float 0.1))
          "Cofactor(3,2) should be 105" 105. cofactor_3_2;
        Alcotest.(check (Alcotest.float 0.01))
          "B[2,3] should be 105/532" (105. /. 532.) (Mat.get b 2 3);
        Alcotest.(check matrix_testable)
          "Inverse matrix should match expected" expected b );
      ( "Calculating the inverse of another matrix" -: fun () ->
        let s =
          "| 8 | -5 | 9 | 2 |\n\
           | 7 | 5 | 6 | 1 |\n\
           | -6 | 0 | 9 | 6 |\n\
           | -3 | 0 | -9 | -4 |"
        in
        let a = Mat.mat_of_str s in
        let inv_a = Mat.inverse a in
        let expected_s =
          "| -0.15385 | -0.15385 | -0.28205 | -0.53846 |\n\
           | -0.07692 | 0.12308 | 0.02564 | 0.03077 |\n\
           | 0.35897 | 0.35897 | 0.43590 | 0.92308 |\n\
           | -0.69231 | -0.69231 | -0.76923 | -1.92308 |"
        in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check matrix_testable)
          "Inverse matrix should match expected" expected inv_a );
      ( "Calculating the inverse of a third matrix" -: fun () ->
        let s =
          "| 9 | 3 | 0 | 9 |\n\
           | -5 | -2 | -6 | -3 |\n\
           | -4 | 9 | 6 | 4 |\n\
           | -7 | 6 | 6 | 2 |"
        in
        let a = Mat.mat_of_str s in
        let inv_a = Mat.inverse a in
        let expected_s =
          "| -0.04074 | -0.07778 | 0.14444 | -0.22222 |\n\
           | -0.07778 | 0.03333 | 0.36667 | -0.33333 |\n\
           | -0.02901 | -0.14630 | -0.10926 | 0.12963 |\n\
           | 0.17778 | 0.06667 | -0.26667 | 0.33333 |"
        in
        let expected = Mat.mat_of_str expected_s in
        Alcotest.(check matrix_testable)
          "Inverse matrix should match expected" expected inv_a );
      ( "Multiplying a product by its inverse" -: fun () ->
        let s_a =
          "| 3 | -9 | 7 | 3 |\n\
           | 3 | -8 | 2 | -9 |\n\
           | -4 | 4 | 4 | 1 |\n\
           | -6 | 5 | -1 | 1 |"
        in
        let a = Mat.mat_of_str s_a in
        let s_b =
          "| 8 | 2 | 2 | 2 |\n\
           | 3 | -1 | 7 | 0 |\n\
           | 7 | 0 | 5 | 4 |\n\
           | 6 | -2 | 0 | 5 |"
        in
        let b = Mat.mat_of_str s_b in
        let c = Mat.mult a b in
        let inv_b = Mat.inverse b in
        let result = Mat.mult c inv_b in
        Alcotest.(check matrix_testable)
          "C * inverse(B) should equal A" a result );
    ] )
