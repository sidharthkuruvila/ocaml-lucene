let () =
  Alcotest.run "Data Intput tests"
  [
    "Mem mapped data sources", Test_mmapped_file_bytes.tests;
    "String data sources", Test_string_bytes.tests;
  ]