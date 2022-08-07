let () =
  Alcotest.run "Test terms enumerator"
  [
(*     "Block pointer", Test_block_pointer.tests;*)
(*     "Block terms dict", Test_block_terms_dict.tests;*)
(*     "Lowercase ascii compression", Test_lowercase_ascii_compression.tests;*)
      "Lz4 compression", Test_lz4_compression.tests;
  ]