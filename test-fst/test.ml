let () =
  Alcotest.run "Fst unit tests"
  [
    "fst", Test_fst.tests;
  ]
