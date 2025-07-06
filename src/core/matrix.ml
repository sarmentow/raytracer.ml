open Common.Common

module Mat = struct
  type t = { rows : int; cols : int; d : float array array }

  let comb float_string float_list =
    Array.append [| float_of_string (String.trim float_string) |] float_list

  let array_from_row_helper arr = Array.fold_right comb arr [||]

  let array_from_row (s : string) =
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char '|' |> Array.of_list |> array_from_row_helper

  let array_is_rect arr =
    Array.for_all (fun a -> Array.length a = Array.length arr.(0)) arr

  let cleanup_repr s =
    String.split_on_char '\n' s |> List.map String.trim |> Array.of_list

  (* Converts the book's matrix string representation to an actual matrix object 
      e.g. 
      | 1    | 2    | 3    | 4    |
      | 5.5  | 6.5  | 7.5  | 8.5  |
      | 9    | 10   | 11   | 12   |
      | 13.5 | 14.5 | 15.5 | 16.5 |

    *)
  let mat_of_str s =
    let d = Array.map array_from_row (cleanup_repr s) in
    if array_is_rect d then
      { rows = Array.length d; cols = Array.length d.(0); d }
    else raise (Invalid_argument "Array is not rectangular")

  let mat_of_array arr =
    if array_is_rect arr then
      { rows = Array.length arr; cols = Array.length arr.(0); d = arr }
    else raise (Invalid_argument "Array is not rectangular")

  let compare_matrices m1 m2 =
    Array.for_all2 (fun a b -> Array.for_all2 (fun i j -> i =. j) a b) m1.d m2.d

  let transpose m =
    let transposed = Array.init_matrix m.cols m.rows (fun i j -> m.d.(j).(i)) in
    { rows = m.cols; cols = m.rows; d = transposed }

  let dot a1 a2 =
    Array.fold_left
      (fun acc a -> acc +. a)
      0.
      (Array.map2 (fun i j -> i *. j) a1 a2)

  (* This could be much more efficient *)
  let mult m1 m2 =
    let t_m2 = transpose m2 in
    let result_matrix = Array.make_matrix m1.rows m2.cols 0. in
    for i = 0 to m1.rows - 1 do
      for j = 0 to m2.cols - 1 do
        result_matrix.(i).(j) <- dot m1.d.(i) t_m2.d.(j)
      done
    done;
    { rows = m1.rows; cols = m2.cols; d = result_matrix }
end
