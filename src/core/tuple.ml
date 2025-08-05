open Common.Common
open Matrix

module type Vec4 = sig
  type t = Mat.t

  val x : t -> float
  val y : t -> float
  val z : t -> float
  val w : t -> float
  val init : float -> float -> float -> float -> t
end

module Vec4Ops (M : Vec4) = struct
  let unite_components f a b =
    M.init
      (f (M.x a) (M.x b))
      (f (M.y a) (M.y b))
      (f (M.z a) (M.z b))
      (f (M.w a) (M.w b))

  let apply_to_components f a scalar =
    M.init
      (f (M.x a) scalar)
      (f (M.y a) scalar)
      (f (M.z a) scalar)
      (f (M.w a) scalar)

  let origin = M.init 0. 0. 0. 1.
  let ( <+> ) a b = unite_components ( +. ) a b
  let ( <-> ) a b = unite_components ( -. ) a b
  let ( ~<-> ) a = unite_components ( -. ) origin a
  let ( <*.> ) a scalar = apply_to_components ( *. ) a scalar
  let ( </.> ) a scalar = apply_to_components ( /. ) a scalar
  let is_point v = M.w v =. 1.0
  let is_vector v = M.w v =. 0.
  let magnitude a = sqrt ((M.x a ** 2.) +. (M.y a ** 2.) +. (M.z a ** 2.))

  let normalize a =
    let m = magnitude a in
    M.init (M.x a /. m) (M.y a /. m) (M.z a /. m) (M.w a /. m)

  let add_components a = M.x a +. M.y a +. M.z a +. M.w a
  let dot a b = unite_components ( *. ) a b |> add_components

  let cross a b =
    let x = (M.y a *. M.z b) -. (M.z a *. M.y b) in
    let y = (M.z a *. M.x b) -. (M.x a *. M.z b) in
    let z = (M.x a *. M.y b) -. (M.y a *. M.x b) in
    M.init x y z 0.

  let init_point x y z = M.init x y z 1.
  let init_vector x y z = M.init x y z 0.
  let ( = ) a b = M.x a = M.x b && M.y a = M.y b && M.z a = M.z b
end

module Tuple : Vec4 = struct
  type t = Mat.t

  let x t = Mat.get t 0 0
  let y t = Mat.get t 1 0
  let z t = Mat.get t 2 0
  let w t = Mat.get t 3 0

  let init x y z w =
    let mat = Mat.create 4 1 in
    Mat.set mat 0 0 x;
    Mat.set mat 1 0 y;
    Mat.set mat 2 0 z;
    Mat.set mat 3 0 w;
    mat
end

module TupleOperations = Vec4Ops (Tuple)
