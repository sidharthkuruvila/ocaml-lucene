let () =
  Alcotest.run "Segment files"
  [
    "Segment file reader", Test_segment_file_reader.tests;
    "Field infos reader", Test_field_infos_reader.tests;
    "Terms meta file reader", Test_meta_file_reader.tests;
  ]