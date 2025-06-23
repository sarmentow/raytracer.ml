
open Raytracer.Matrix
let (-:) name f = Alcotest.test_case name `Quick f
let tests = "chapter3", [
  "splits a valid matrix into its constituent rows" -: begin fun () -> 
    let s = "  | 1    | 2    | 3    | 4    |
      | 5.5  | 6.5  | 7.5  | 8.5  |
      | 9    | 10   | 11   | 12   |
      | 13.5 | 14.5 | 15.5 | 16.5 |  " in
    Alcotest.(check (array string)) "splits it into rows" 
      [|"| 1    | 2    | 3    | 4    |"; 
      "| 5.5  | 6.5  | 7.5  | 8.5  |";
      "| 9    | 10   | 11   | 12   |";
      "| 13.5 | 14.5 | 15.5 | 16.5 |"|] (Mat.cleanup_repr s) 
  end;

  "correctly transforms a matrix from its string representation" -: begin fun() ->
    let s = "  | 1    | 2    | 3    | 4    |
        | 5.5  | 6.5  | 7.5  | 8.5  |
        | 9    | 10   | 11   | 12   |
        | 13.5 | 14.5 | 15.5 | 16.5 |  " in
    Alcotest.(check bool) "turns it into a matrix" true
      (Mat.compare_matrices (Mat.mat_of_repr s) Mat.mat_of_array([|
          [|1.; 2.; 3.; 4.;|];
          [|5.5; 6.5; 7.5; 8.5;|];
          [|9.; 10.; 11.; 12.;|];
          [|13.5; 14.5; 15.5; 16.5;|]
        |]))
  end;
]