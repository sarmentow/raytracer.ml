type t = { time : float; obj : Sphere.t }

let init time s = { time; obj = s }

let hit intersections =
  intersections
  |> List.filter (fun intersection -> intersection.time >= 0.)
  |> List.sort (fun a b -> compare a.time b.time)
  |> function
  | [] -> None
  | x :: _ -> Some x
