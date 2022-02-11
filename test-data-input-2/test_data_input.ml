open Lucene_data_input_2


(*
 Copied from
 https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string  *)
let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
let input = read_whole_file "data/data-input.bytes"

module M = Bytes_source.Make(String_bytes)
module N = Data_input.Make(M)

let test_read_int () =
  let di = M.of_bytes input in
  let result = N.read_int di in
  let expected = 24 in
  Alcotest.(check int) "The read int should be 24" expected result;
  let result = N.read_int di in
  let expected = 98237 in
  Alcotest.(check int) "The read int should be 98237" expected result;
  let result = N.read_int di in
  let expected = 2147483647 in
  Alcotest.(check int) "The read int should be 2147483647" expected result;
  let result = N.read_int di in
  let expected = -2147483648 in
  Alcotest.(check int) "The read int should be 98237" expected result;
  let expected_position = 16 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_long () =
  let di = M.of_bytes input in
  M.set_position di 16;
  let result = N.read_long di in
  let expected = 24 in
  Alcotest.(check int) "The read long should be 24" expected result;
  let result = N.read_long di in
  let expected = 98237 in
  Alcotest.(check int) "The read long should be 98237" expected result;
  let result = N.read_long di in
  let expected = 2147483647 in
  Alcotest.(check int) "The read long should be 2147483647" expected result;
  let result = N.read_long di in
  let expected = -2147483648 in
  Alcotest.(check int) "The read long should be -2147483648" expected result;
  let result = N.read_long di in
  let expected = 4611686018427387903 in
  Alcotest.(check int) "The read long should be 4611686018427387903" expected result;
  let result = N.read_long di in
  let expected = -4611686018427387904 in
  Alcotest.(check int) "The read long should be -4611686018427387904" expected result;
  let expected_position = 64 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_vint () =
  let di = M.of_bytes input in
  M.set_position di 64;
  let result = N.read_vint di in
  let expected = 24 in
  Alcotest.(check int) "The read int should be 24" expected result;
  let result = N.read_vint di in
  let expected = 98237 in
  Alcotest.(check int) "The read int should be 98237" expected result;
  let result = N.read_vint di in
  let expected = 2147483647 in
  Alcotest.(check int) "The read int should be 2147483647" expected result;
  let result = N.read_vint di in
  let expected = -2147483648 in
  Alcotest.(check int) "The read int should be -2147483648" expected result;
  let expected_position = 78 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_vlong () =
  let di = M.of_bytes input in
  M.set_position di 78;
  let result = N.read_vlong di in
  let expected = 24 in
  Alcotest.(check int) "The read long should be 24" expected result;
  let result = N.read_vlong di in
  let expected = 98237 in
  Alcotest.(check int) "The read long should be 98237" expected result;
  let result = N.read_vlong di in
  let expected = 2147483647 in
  Alcotest.(check int) "The read long should be 2147483647" expected result;
  let result = N.read_vlong di in
  let expected = 4611686018427387903 in
  Alcotest.(check int) "The read long should be 4611686018427387903" expected result;
  let expected_position = 96 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_string () =
  let di = M.of_bytes input in
  M.set_position di 96;
  let result = N.read_string di in
  let expected = "hello world" in
  Alcotest.(check string) "The read string should be \"hello world\"" expected result;
  let expected_position = 108 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_list_of_strings () =
  let di = M.of_bytes input in
  M.set_position di 108;
  let result = N.read_list_of_strings di in
  let expected = ["hello"; "world"] in
  Alcotest.(check (list string)) "The read list should be [\"hello\"; \"world\"]" expected result;
  let expected_position = 121 in
  let updated_position = M.get_position di in
  Alcotest.(check int) "Position should be updated" expected_position updated_position

let test_read_assoc_list_of_strings () =
  let di = M.of_bytes input in
  M.set_position di 121;
  let result : (string * string) list = N.read_assoc_list_of_strings di in
  let expected : (string * string) list  = ["world", "mundo"; "hello", "hola"] in
  Alcotest.(check (list (pair string string))) "The read alist should be [\"hello\", \"hola\"; \"world\", \"mundo\"]" expected result
let tests = [
  "Read an int from the bytes source", `Quick, test_read_int;
  "Read a long from the bytes source", `Quick, test_read_long;
  "Read a vint from the bytes source", `Quick, test_read_vint;
  "Read a vlong from the bytes source", `Quick, test_read_vlong;
  "Read a string from the bytes source", `Quick, test_read_string;
  "Read a list of strings from the bytes source", `Quick, test_read_list_of_strings;
  "Reat an assoc list of strings from the bytes source", `Quick, test_read_assoc_list_of_strings;
]

