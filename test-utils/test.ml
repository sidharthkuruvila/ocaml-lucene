let () =
  Alcotest.run "Search data tool unit tests"
  [
    "bit utils", Test_bit_utils.tests;
  ]
