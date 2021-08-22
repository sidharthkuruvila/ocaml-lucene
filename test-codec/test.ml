let () =
  Alcotest.run "Search data tool unit tests"
  [
    "read_segment", Test_bit_set_util.tests;
  ]
