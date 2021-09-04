let () =
  Alcotest.run "Search data tool unit tests"
  [
    "bit sets", Test_bit_set_util.tests;
    "hex utils", Test_hex_util.tests;
  ]
