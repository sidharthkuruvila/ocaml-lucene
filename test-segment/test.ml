let () =
  Alcotest.run "Segment files"
  [
    "Segment file readeer", Test_segment_file_reader.tests;
  ]