open Lucene_data_input_2

module Sliced_bytes = Sliced_bytes.Make(String_bytes)

let s = "This is a string"
let sliced = Sliced_bytes.slice s 3 5

let test_length () =
  let result = Sliced_bytes.length sliced in
  let expected = 5 in
  Alcotest.(check int) "Length should be 5" expected result

let test_get_byte () =
  let result = Sliced_bytes.get_byte sliced 0 in
  let expected = 's' in
  Alcotest.(check char) "Get byte 0 should return 's'" expected result

let test_copy_bytes () =
  let bytes = Bytes.create 3 in
  Sliced_bytes.copy_bytes sliced bytes ~src_index:2 ~dest_index:0 ~length:3;
  let result = String.of_bytes bytes in
  let expected = "is " in
  Alcotest.(check string) "Copy a subtring 'is ' into a bytes" expected result


let tests = [
  "Length of a sliced bytes", `Quick, test_length;
  "Get a byte from a sliced bytes", `Quick, test_get_byte;
  "Get copy an array of byres from a sliced bytes", `Quick, test_copy_bytes;
]