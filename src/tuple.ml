open Common

type t = Matrix.t

let x t = Matrix.get t 0 0
let y t = Matrix.get t 1 0
let z t = Matrix.get t 2 0
let w t = Matrix.get t 3 0

let init x y z w =
  let mat = Matrix.create 4 1 in
  Matrix.set mat 0 0 x;
  Matrix.set mat 1 0 y;
  Matrix.set mat 2 0 z;
  Matrix.set mat 3 0 w;
  mat

let unite_components f a b =
  init (f (x a) (x b)) (f (y a) (y b)) (f (z a) (z b)) (f (w a) (w b))

let apply_to_components f a scalar =
  init (f (x a) scalar) (f (y a) scalar) (f (z a) scalar) (f (w a) scalar)

let origin = init 0. 0. 0. 0.
let ( <+> ) a b = unite_components ( +. ) a b
let ( <-> ) a b = unite_components ( -. ) a b
let ( ~<-> ) a = unite_components ( -. ) origin a
let ( <*.> ) a scalar = apply_to_components ( *. ) a scalar
let ( </.> ) a scalar = apply_to_components ( /. ) a scalar
let is_point v = w v =. 1.0
let is_vector v = w v =. 0.
let magnitude a = sqrt ((x a ** 2.) +. (y a ** 2.) +. (z a ** 2.))

let normalize a =
  let m = magnitude a in
  init (x a /. m) (y a /. m) (z a /. m) (w a /. m)

let add_components a = x a +. y a +. z a +. w a
let dot a b = unite_components ( *. ) a b |> add_components

let cross a b =
  let cross_x = (y a *. z b) -. (z a *. y b) in
  let cross_y = (z a *. x b) -. (x a *. z b) in
  let cross_z = (x a *. y b) -. (y a *. x b) in
  init cross_x cross_y cross_z 0.

let init_point x y z = init x y z 1.
let init_vector x y z = init x y z 0.
let ( = ) a b = x a =. x b && y a =. y b && z a =. z b && w a =. w b
