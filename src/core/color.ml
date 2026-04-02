module Color : Tuple.Vec4 = struct
  type t = Matrix.t

  let x t = Matrix.get t 0 0
  let y t = Matrix.get t 1 0
  let z t = Matrix.get t 2 0
  let w t = Matrix.get t 3 0

  let init red green blue alpha =
    let mat = Matrix.create 4 1 in
    Matrix.set mat 0 0 red;
    Matrix.set mat 1 0 green;
    Matrix.set mat 2 0 blue;
    Matrix.set mat 3 0 alpha;
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
