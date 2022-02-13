let () =
  Alcotest.run "Data output tests"
  [
    "Buffer data output", Test_buffer_bytes_writer.tests;
    "Data output", Test_data_output.tests;
  ]