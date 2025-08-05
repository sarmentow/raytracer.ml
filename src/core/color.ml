open Matrix

module Color : Tuple.Vec4 = struct
  type t = Mat.t

  let x t = Mat.get t 0 0
  let y t = Mat.get t 1 0
  let z t = Mat.get t 2 0
  let w t = Mat.get t 3 0

  let init red green blue alpha =
    let mat = Mat.create 4 1 in
    Mat.set mat 0 0 red;
    Mat.set mat 1 0 green;
    Mat.set mat 2 0 blue;
    Mat.set mat 3 0 alpha;
    mat
end

module ColorOps = struct
  include Tuple.Vec4Ops (Color)

  let ( <*> ) a b =
    Color.init
      (Color.x a *. Color.x b)
      (Color.y a *. Color.y b)
      (Color.z a *. Color.z b)
      (Color.w a *. Color.w b)
end
