open Lucene_data_output

let test_writing_a_single_byte () =
  let buffer = Buffer.create 10 in
  Buffer_bytes_writer.write_byte buffer 'a';
  Buffer_bytes_writer.write_byte buffer 'b';
  Buffer_bytes_writer.write_byte buffer 'c';
  let expected = "abc" in
  let result = Buffer.contents buffer in
  Alcotest.(check string) "The written string should be \"abc\"" expected result

let test_the_length () =
  let buffer = Buffer.create 10 in
  Buffer_bytes_writer.write_byte buffer 'a';
  Buffer_bytes_writer.write_byte buffer 'b';
  Buffer_bytes_writer.write_byte buffer 'c';
  let expected = 3 in
  let result = Buffer_bytes_writer.length buffer in
  Alcotest.(check int) "The length of the buffer should be 3" expected result

let tests = [
  "Write a single byte", `Quick, test_writing_a_single_byte;
]