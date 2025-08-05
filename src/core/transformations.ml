open Matrix

module Transform = struct
  let translation x y z =
    let arr =
      [| 1.; 0.; 0.; x; 0.; 1.; 0.; y; 0.; 0.; 1.; z; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4

  let scale x y z =
    let arr =
      [| x; 0.; 0.; 0.; 0.; y; 0.; 0.; 0.; 0.; z; 0.; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4

  let rotation_x rad =
    let c = Stdlib.cos rad in
    let s = Stdlib.sin rad in
    let arr =
      [| 1.; 0.; 0.; 0.; 0.; c; -.s; 0.; 0.; s; c; 0.; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4

  let rotation_y rad =
    let c = Stdlib.cos rad in
    let s = Stdlib.sin rad in
    let arr =
      [| c; 0.; s; 0.; 0.; 1.; 0.; 0.; -.s; 0.; c; 0.; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4

  let rotation_z rad =
    let c = Stdlib.cos rad in
    let s = Stdlib.sin rad in
    let arr =
      [| c; -.s; 0.; 0.; s; c; 0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4

  let shearing xy xz yx yz zx zy =
    let arr =
      [| 1.; xy; xz; 0.; yx; 1.; yz; 0.; zx; zy; 1.; 0.; 0.; 0.; 0.; 1. |]
    in
    Mat.mat_of_arr arr 4 4
end
