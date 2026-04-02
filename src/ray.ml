type t = { origin : Tuple.t; direction : Tuple.t }

let init origin direction = { origin; direction }

let pp fmt r =
  Format.fprintf fmt "Ray(%a, %a)" Tuple.pp r.origin Tuple.pp r.direction

let ( = ) r1 r2 =
  Tuple.( = ) r1.origin r2.origin && Tuple.( = ) r1.direction r2.direction

let position r time =
  let displacement = Tuple.( <*.> ) r.direction time in
  Tuple.( <+> ) r.origin displacement

let transform m r =
  { origin = Matrix.mult m r.origin; direction = Matrix.mult m r.direction }

let intersect (s : Sphere.t) r =
  let r2 = transform (Matrix.inverse s.transform) r in
  let sphere_to_ray = Tuple.( <-> ) r2.origin (Tuple.init_point 0. 0. 0.) in
  let a = Tuple.dot r2.direction r2.direction in
  let b = 2. *. Tuple.dot r2.direction sphere_to_ray in
  let c = Tuple.dot sphere_to_ray sphere_to_ray -. 1. in
  let discriminant = (b ** 2.) -. (4. *. a *. c) in
  if discriminant < 0. then []
  else
    let delta = sqrt discriminant in
    let t1 = (-.b -. delta) /. (2. *. a) in
    let t2 = (-.b +. delta) /. (2. *. a) in
    [ Intersection.init t1 s; Intersection.init t2 s ]
