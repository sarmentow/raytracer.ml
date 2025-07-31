open Common.Common

module Mat = struct
  type t = {
    rows : int;
    cols : int;
    stride_r : int; (* Jump to get to the next row *)
    stride_c : int; (* Jump to get to the next col *)
    offset : int; (* Start index in the data array *)
    d : float array;
  }

  let get m i j = m.d.(m.offset + (i * m.stride_r) + (j * m.stride_c))
  let set m i j v = m.d.(m.offset + (i * m.stride_r) + (j * m.stride_c)) <- v

  let create rows cols =
    {
      rows;
      cols;
      stride_r = cols;
      stride_c = 1;
      offset = 0;
      d = Array.make (rows * cols) 0.;
    }

  let copy m =
    {
      rows = m.rows;
      cols = m.cols;
      stride_r = m.stride_r;
      stride_c = m.stride_c;
      offset = m.offset;
      d = Array.copy m.d;
    }

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

  let array_of_matrix matrix = Array.fold_left Array.append [||] matrix

  (* Converts the book's matrix string representation to an actual matrix object 
      e.g. 
      | 1    | 2    | 3    | 4    |
      | 5.5  | 6.5  | 7.5  | 8.5  |
      | 9    | 10   | 11   | 12   |
      | 13.5 | 14.5 | 15.5 | 16.5 |

    *)
  let mat_of_str s =
    let d = Array.map array_from_row (cleanup_repr s) in
    let final_arr = d |> array_of_matrix in
    if array_is_rect d then
      {
        rows = Array.length d;
        cols = Array.length d.(0);
        d = final_arr;
        stride_r = Array.length d.(0);
        stride_c = 1;
        offset = 0;
      }
    else raise (Invalid_argument "Array is not rectangular")

  let mat_of_matrix arr =
    let final_arr = Array.fold_left Array.append [||] arr in
    if array_is_rect arr then
      {
        rows = Array.length arr;
        cols = Array.length arr.(0);
        d = final_arr;
        stride_r = Array.length arr.(0);
        stride_c = 1;
        offset = 0;
      }
    else raise (Invalid_argument "Array is not rectangular")

  let compare_matrices m1 m2 =
    if m1.rows <> m2.rows || m1.cols <> m2.cols then false
    else
      let rec compare_elements i j =
        if i >= m1.rows then true
        else if j >= m1.cols then compare_elements (i + 1) 0
        else get m1 i j =. get m2 i j && compare_elements i (j + 1)
      in
      compare_elements 0 0

  let transpose m =
    {
      rows = m.cols;
      cols = m.rows;
      d = m.d;
      stride_r = 1;
      stride_c = m.rows;
      offset = 0;
    }

  let mult m1 m2 =
    if m1.cols <> m2.rows then raise (Invalid_argument "Invalid Arguments")
    else
      let result = create m1.rows m2.cols in
      for i = 0 to m1.rows - 1 do
        for j = 0 to m2.cols - 1 do
          let sum = ref 0. in
          for k = 0 to m1.cols - 1 do
            sum := !sum +. (get m1 i k *. get m2 k j)
          done;
          set result i j !sum
        done
      done;
      result

  let submat m i j =
    if i >= m.rows || j >= m.cols || i < 0 || j < 0 then
      raise (Invalid_argument "Invalid row/col for deletion")
    else
      let new_rows = m.rows - 1 in
      let new_cols = m.cols - 1 in
      let result = create new_rows new_cols in
      for dst_i = 0 to new_rows - 1 do
        let src_i = if dst_i < i then dst_i else dst_i + 1 in
        for dst_j = 0 to new_cols - 1 do
          let src_j = if dst_j < j then dst_j else dst_j + 1 in
          let src_val = get m src_i src_j in
          set result dst_i dst_j src_val
        done
      done;
      result

  let rec det m =
    if m.rows = 1 && m.cols = 1 then get m 0 0
    else if m.rows <> m.cols then
      raise (Invalid_argument "matrix is non-square")
    else if m.rows = 2 && m.cols = 2 then
      (get m 0 0 *. get m 1 1) -. (get m 0 1 *. get m 1 0)
    else
      let rec sum_cofactors j acc =
        if j >= m.cols then acc
        else
          let cof = get m 0 j *. cofactor m 0 j in
          sum_cofactors (j + 1) (acc +. cof)
      in
      sum_cofactors 0 0.

  and minor m i j = det (submat m i j)
  and cofactor m i j = minor m i j *. if (i + j) mod 2 = 0 then 1. else -1.

  let inverse m =
    let det_m = det m in
    if abs_float det_m < epsilon then
      raise (Invalid_argument "m is not invertible")
    else
      let result = create m.rows m.cols in
      for row = 0 to m.rows - 1 do
        for col = 0 to m.cols - 1 do
          let c = cofactor m row col in
          set result col row (c /. det_m)
        done
      done;
      result
end
