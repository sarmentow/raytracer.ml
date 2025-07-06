module Common = struct
  let epsilon = 0.001
  let ( =. ) a b = Float.abs (a -. b) < epsilon
end
