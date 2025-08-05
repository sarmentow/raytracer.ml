let () =
  Alcotest.run "Raytracer"
    [ Chapter1.tests; Chapter2.tests; Chapter3.tests; Chapter4.tests ]
