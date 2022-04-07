let () =
  Alcotest.run "Test terms enumerator"
  [
     "Floor data", Test_block_pointer.tests;
  ]