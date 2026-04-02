include Tuple

let ( <*> ) a b = init (x a *. x b) (y a *. y b) (z a *. z b) (w a *. w b)
