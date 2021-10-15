let () =
  Alcotest.run "Bit packing tess"
  [
    "bit packing", Test_bit_packing.tests;
  ]
