module Color : Tuple.Vec4 = struct
  type t = {red: float; green: float; blue: float; alpha: float}
  let x t = t.red
  let y t = t.green
  let z t = t.blue
  let w t = t.alpha
  let init red green blue alpha = {red; green; blue; alpha}
end

module ColorOps = struct
  include Tuple.Vec4Ops(Color)
  let (<*>) a b = 
    Color.init ((Color.x a) *. (Color.x b)) 
               ((Color.y a) *. (Color.y b)) 
               ((Color.z a) *. (Color.z b)) 
               ((Color.w a) *. (Color.w b)) 
end