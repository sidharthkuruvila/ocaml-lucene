
open Lucene_data_input_2

let input = "This is a simple test text file."

module M = Reverse_bytes_source.Make(String_bytes)


let test_read_byte () =
  let src = M.of_bytes input in
  let result = M.read_byte src in
  let expected = '.' in
  Alcotest.(check char) "Should read the first byte 'T'" expected result;
  let expected_position = 30 in
  let updated_position = M.get_position src in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_bytes () =
  let src = M.of_bytes input in
  let result = M.read_bytes src 16 in
  let expected = " test text file." in
  Alcotest.(check string) "Should read the string \" test text file.\"" expected result;
  let expected_position = 15 in
  let updated_position = M.get_position src in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_get_byte () =
  let src = M.of_bytes input in
  let result = M.get_byte src 16 in
  let expected = 'e' in
  Alcotest.(check char) "Should read the string \" test text file.\"" expected result;
  let expected_position = 31 in
  let updated_position = M.get_position src in
  Alcotest.(check int) "Position should not be updated" expected_position updated_position

let test_skip_bytes () =
  let src = M.of_bytes input in
  M.skip_bytes src 15;
  let expected_position = 16 in
  let updated_position = M.get_position src in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_set_position () =
  let src = M.of_bytes input in
  M.set_position src 15;
  let expected_position = 15 in
  let updated_position = M.get_position src in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_copy () =
  let src = M.of_bytes input in
  let src_copy = M.copy src in
  M.set_position src 15;
  let expected_position = 31 in
  let updated_position = M.get_position src_copy in
  Alcotest.(check int) "Position in copy should not be updated" expected_position updated_position

let tests = [
  "Read a byte from a source", `Quick, test_read_byte;
  "Read a string from a source", `Quick, test_read_bytes;
  "Get a byte from the source without updating the position", `Quick, test_get_byte;
  "Skip bytes and update the position", `Quick, test_skip_bytes;
  "Update the current position", `Quick, test_set_position;
  "Test copying the source", `Quick, test_copy;
]