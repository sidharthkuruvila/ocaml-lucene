let () =
  Alcotest.run "Fst unit tests"
  [
    "fst", Test_fst.tests;
    "acyclic transducer", Test_acyclic_transducer.tests;
    "string output", Test_string_output.tests;
  ]
