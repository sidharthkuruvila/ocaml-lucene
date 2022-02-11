let () =
  Alcotest.run "Data Intput tests"
  [
    "Mem mapped data sources", Test_mmapped_file_bytes_source.tests;
    "String data sources", Test_string_bytes_source.tests;
  ]