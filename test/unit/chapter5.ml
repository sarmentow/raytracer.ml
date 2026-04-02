open Raytracer

let ray_testable = Alcotest.testable Ray.pp Ray.( = )
let ( -: ) name f = Alcotest.test_case name `Quick f

let tests =
  ( "chapter5",
    [
      ( "Creating a ray" -: fun () ->
        let origin = Tuple.init_point 1. 2. 3. in
        let direction = Tuple.init_vector 4. 5. 6. in
        let expected = Ray.init origin direction in
        (Tuple.( = ) expected.origin origin
        && Tuple.( = ) expected.direction direction)
        |> Alcotest.(check bool) "origin" true );
      (*
        Scenario: Computing a point from a distance
        Given r ← ray(point(2, 3, 4), vector(1, 0, 0))
        Then position(r, 0) = point(2, 3, 4)
        And position(r, 1) = point(3, 3, 4)
        And position(r, -1) = point(1, 3, 4)
        And position(r, 2.5) = point(4.5, 3, 4)
      *)
      ( "Computing a point from a distance" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 2. 3. 4.) (Tuple.init_vector 1. 0. 0.)
        in
        Tuple.( = ) (Ray.position r 0.) (Tuple.init_point 2. 3. 4.)
        |> Alcotest.(check bool) "position(r, 0) = point(2, 3, 4)" true;
        Tuple.( = ) (Ray.position r 1.) (Tuple.init_point 3. 3. 4.)
        |> Alcotest.(check bool) "position(r, 1) = point(3, 3, 4)" true;
        Tuple.( = ) (Ray.position r (-1.)) (Tuple.init_point 1. 3. 4.)
        |> Alcotest.(check bool) "position(r, -1) = point(1, 3, 4)" true;
        Tuple.( = ) (Ray.position r 2.5) (Tuple.init_point 4.5 3. 4.)
        |> Alcotest.(check bool) "position(r, 2.5) = point(4.5, 3, 4)" true );
      ( "A ray intersects a sphere at two points" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 0. 0. (-5.)) (Tuple.init_vector 0. 0. 1.)
        in
        let s = Sphere.init 0 in
        let xs = Ray.intersect s r in
        List.length xs = 2 |> Alcotest.(check bool) "xs.count = 2" true;
        (List.nth xs 0).time = 4.0 |> Alcotest.(check bool) "xs[0] = 4.0" true;
        (List.nth xs 1).time = 6.0 |> Alcotest.(check bool) "xs[1] = 6.0" true
      );
      ( "A ray misses a sphere" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 0. 2. (-5.)) (Tuple.init_vector 0. 0. 1.)
        in
        let s = Sphere.init 0 in
        let xs = Ray.intersect s r in
        List.length xs = 0 |> Alcotest.(check bool) "xs.count = 0" true );
      ( "A ray originates inside a sphere" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 0. 0. 0.) (Tuple.init_vector 0. 0. 1.)
        in
        let s = Sphere.init 0 in
        let xs = Ray.intersect s r in
        List.length xs = 2 |> Alcotest.(check bool) "xs.count = 2" true;
        (List.nth xs 0).time = -1.0 |> Alcotest.(check bool) "xs[0] = -1.0" true;
        (List.nth xs 1).time = 1.0 |> Alcotest.(check bool) "xs[1] = 1.0" true
      );
      ( "A sphere is behind a ray" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 0. 0. 5.) (Tuple.init_vector 0. 0. 1.)
        in
        let s = Sphere.init 0 in
        let xs = Ray.intersect s r in
        List.length xs = 2 |> Alcotest.(check bool) "xs.count = 2" true;
        (List.nth xs 0).time = -6.0 |> Alcotest.(check bool) "xs[0] = -6.0" true;
        (List.nth xs 1).time = -4.0 |> Alcotest.(check bool) "xs[1] = -4.0" true
      );
      ( "An intersection encapsulates t and object" -: fun () ->
        let s = Sphere.init 0 in
        let i = Intersection.init 3.5 s in
        i.time = 3.5 |> Alcotest.(check bool) "i.t = 3.5" true;
        i.obj.id = s.id |> Alcotest.(check bool) "i.object = s" true );
      ( "The hit, when all intersections have positive t" -: fun () ->
        let s = Sphere.init 0 in
        let i1 = Intersection.init 1. s in
        let i2 = Intersection.init 2. s in
        let xs = [ i2; i1 ] in
        let i = Intersection.hit xs |> Option.get in
        i.time = i1.time |> Alcotest.(check bool) "i = i1" true );
      ( "The hit, when some intersections have negative t" -: fun () ->
        let s = Sphere.init 0 in
        let i1 = Intersection.init (-1.) s in
        let i2 = Intersection.init 1. s in
        let xs = [ i2; i1 ] in
        let i = Intersection.hit xs |> Option.get in
        i.time = i2.time |> Alcotest.(check bool) "i = i2" true );
      ( "The hit, when all intersections have negative t" -: fun () ->
        let s = Sphere.init 0 in
        let i1 = Intersection.init (-2.) s in
        let i2 = Intersection.init (-1.) s in
        let xs = [ i2; i1 ] in
        let i = Intersection.hit xs in
        i = None |> Alcotest.(check bool) "i is nothing" true );
      ( "The hit is always the lowest nonnegative intersection" -: fun () ->
        let s = Sphere.init 0 in
        let i1 = Intersection.init 5. s in
        let i2 = Intersection.init 7. s in
        let i3 = Intersection.init (-3.) s in
        let i4 = Intersection.init 2. s in
        let xs = [ i1; i2; i3; i4 ] in
        let i = Intersection.hit xs |> Option.get in
        i.time = i4.time |> Alcotest.(check bool) "i = i4" true );
      ( "Translating a ray" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 1. 2. 3.) (Tuple.init_vector 0. 1. 0.)
        in
        let m = Transformations.translation 3. 4. 5. in
        let r2 = Ray.transform m r in
        Tuple.( = ) r2.origin (Tuple.init_point 4. 6. 8.)
        |> Alcotest.(check bool) "r2.origin = point(4, 6, 8)" true;
        Tuple.( = ) r2.direction (Tuple.init_vector 0. 1. 0.)
        |> Alcotest.(check bool) "r2.direction = vector(0, 1, 0)" true );
      ( "Scaling a ray" -: fun () ->
        let r =
          Ray.init (Tuple.init_point 1. 2. 3.) (Tuple.init_vector 0. 1. 0.)
        in
        let m = Transformations.scale 2. 3. 4. in
        let r2 = Ray.transform m r in
        Tuple.( = ) r2.origin (Tuple.init_point 2. 6. 12.)
        |> Alcotest.(check bool) "r2.origin = point(2, 6, 12)" true;
        Tuple.( = ) r2.direction (Tuple.init_vector 0. 3. 0.)
        |> Alcotest.(check bool) "r2.direction = vector(0, 3, 0)" true );
    ] )
