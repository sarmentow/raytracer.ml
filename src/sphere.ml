type t = { id : int; transform : Matrix.t }

let init id = { id; transform = Matrix.identity }
let transform t s = { id = s.id; transform = t }
