open Lucene_data_input_2


let src = "This is a simple test text file."

let test_getting_a_single_byte () =
  let result = String_bytes_source.get_byte src 10 in
  let expected = 's' in
  Alcotest.(check char) "Char located at index 10 should be 's'" expected result

let test_copying_bytes () =
  let dest = Bytes.of_string "Anythings" in
  String_bytes_source.copy_bytes src dest ~src_index:2 ~dest_index:2 ~length:5;
  let expected = "Anis isgs" in
  let result = Bytes.to_string dest in
  Alcotest.(check string) "Updated string should be \"Anis isgs\"" expected result

let test_length () =
  let result = String_bytes_source.length src in
  let expected = 32 in
  Alcotest.(check int) "Char located at index 10 should be 's'" expected result
let tests = [
  "Getting a single byte", `Quick, test_getting_a_single_byte;
  "Copying into a byte array", `Quick, test_copying_bytes;
  "The length of a mem mapped file", `Quick, test_length;
]