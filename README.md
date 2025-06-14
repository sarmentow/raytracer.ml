# raytracer.ml

dune build

dune runtest

## Questions

### 1.

Why does this work:

```ocaml
let compute_magnitude_test v mag = Printf.sprintf "computing the magnitude of vector (%f, %f, %f)" v.x v.y v.z -: begin fun() ->

    let a1 = v in
    (magnitude a1)
    |> Alcotest.(check (float 0.001)) (Printf.sprintf "magnitude is %f" mag) mag
end
```

But not this:

```ocaml
let compute_magnitude_test v mag = Printf.sprintf "computing the magnitude of vector (%f, %f, %f)" v.x v.y v.z -: begin fun() ->
    let a1 = v in
    (magnitude a1 =. mag)
    |> Alcotest.(check bool) "magnitude" true
end

```

---

### 2.

Another quirk. Say I have the following function:

```ocaml
let (<*.>) a b = apply_to_components ( *. ) a b
```

and `apply_to_components` is

```ocaml
let apply_to_components f a scalar = init_tuple (f a.x scalar) (f a.y scalar) (f a.z scalar) (f a.w scalar)
```

How could I make it so that I could pass the float and the tuple in any order? Would that be an antipattern or perfectly okay thing to do?
