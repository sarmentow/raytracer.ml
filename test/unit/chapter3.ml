open Raytracer.Matrix

let ( -: ) name f = Alcotest.test_case name `Quick f

let matrix_testable =
  let pp fmt (m : Mat.t) = Format.fprintf fmt "Matrix %dx%d" m.rows m.cols in
  let equal m1 m2 = Mat.compare_matrices m1 m2 in
  Alcotest.testable pp equal

let float_array_testable = Alcotest.array (Alcotest.array (Alcotest.float 0.1))

let tests =
  ( "chapter3",
    [
      ( "correctly transforms a matrix from its string representation"
      -: fun () ->
        let s =
          "  | 1    | 2    | 3    | 4    |\n\
          \        | 5.5  | 6.5  | 7.5  | 8.5  |\n\
          \        | 9    | 10   | 11   | 12   |\n\
          \        | 13.5 | 14.5 | 15.5 | 16.5 |  "
        in
        let arr_mat =
          Mat.mat_of_array
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
          \        | 5.5  | 6.5  | 7.5  | 8.5  |\n\
          \        | 9    | 10   | 11   | 12   |\n\
          \        | 13.5 | 14.5 | 15.5 | 16.5 |  "
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
        let float_array_testable =
          Alcotest.array (Alcotest.array (Alcotest.float 0.1))
        in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d arr );
      ( "Constructing and inspecting a 2x2 matrix" -: fun () ->
        let s = " | -3 | 5 |\n              | 1 | -2 | " in
        let s_mat = Mat.mat_of_str s in
        let arr = [| [| -3.; 5. |]; [| 1.; -2. |] |] in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d arr );
      ( "Constructing and inspecting a 3x3 matrix" -: fun () ->
        let s =
          " | -3 | 5 | 0  |\n\
          \              | 1 | -2 | -7 | \n\
          \              | 0 | 1  | 1  |"
        in
        let s_mat = Mat.mat_of_str s in
        let arr =
          [| [| -3.; 5.; 0. |]; [| 1.; -2.; -7. |]; [| 0.; 1.; 1. |] |]
        in
        Alcotest.(check float_array_testable)
          "Matrix values should match array" s_mat.d arr );
      ( "Comparing matrices" -: fun () ->
        let s =
          "| 1 | 2 | 3 | 4 |\n\
          \              | 5 | 6 | 7 | 8 |\n\
          \              | 9 | 8 | 7 | 6 |\n\
          \              | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s in
        let b = Mat.mat_of_str s in
        Alcotest.(check bool)
          "Matrices from the same string representation should match" true
          (Mat.compare_matrices a b) );
      ( "Comparing matrices" -: fun () ->
        let s1 =
          "| 1 | 2 | 3 | 4 |\n\
          \                | 5 | 6 | 7 | 8 |\n\
          \                | 9 | 8 | 7 | 6 |\n\
          \                | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s1 in
        let s2 =
          "| 2 | 3 | 4 | 5 |\n\
          \                | 6 | 7 | 8 | 9 |\n\
          \                | 8 | 7 | 6 | 5 |\n\
          \                | 4 | 3 | 2 | 1 |"
        in
        let b = Mat.mat_of_str s2 in
        Alcotest.(check bool)
          "Matrices from different string representations should not match"
          false (Mat.compare_matrices a b) );
      ( "Multiplying matrices" -: fun () ->
        let s1 =
          "| 1 | 2 | 3 | 4 |\n\
          \                | 5 | 6 | 7 | 8 |\n\
          \                | 9 | 8 | 7 | 6 |\n\
          \                | 5 | 4 | 3 | 2 |"
        in
        let a = Mat.mat_of_str s1 in
        let s2 =
          "| -2 | 1 | 2 | 3 |\n\
          \                | 3 | 2 | 1 | -1 |\n\
          \                | 4 | 3 | 6 | 5 |\n\
          \                | 1 | 2 | 7 | 8 |"
        in
        let b = Mat.mat_of_str s2 in
        let s3 =
          "| 20| 22 | 50 | 48 |\n\
          \                | 44| 54 | 114 | 108 |\n\
          \                | 40| 58 | 110 | 102 |\n\
          \                | 16| 26 | 46 | 42 |"
        in
        let expected_result = Mat.mat_of_str s3 in
        Alcotest.(check matrix_testable)
          "Matrix mult result is not the expected"
          expected_result (Mat.mult a b) );
    ] )
