open Lucene_data_output
open Lucene_data_input_2


module Buffer_data_output = Data_output.Make(Buffer_bytes_writer)
module String_bytes_source = Bytes_source.Make(String_bytes)
module String_data_input = Data_input.Make(String_bytes_source)

let test_write_a_vint () =
  let nums = [
    10;
    256;
    1024;
    7462834;
    Int32.max_int |> Int32.to_int;
    -1;
    -10;
    Int32.min_int |> Int32.to_int;
  ] in
  let buffer = Buffer.create 10 in
  List.iter (fun n ->
    Buffer_data_output.write_vint buffer n;
  ) nums;
  let s = Buffer.contents buffer in
  let source = String_bytes_source.of_bytes s in
  List.iter (fun expected ->
    let result = String_data_input.read_vint source in
    let test_message = Printf.sprintf "Retrieved number should be %d" expected in
    Alcotest.(check int) test_message expected result
  ) nums;
  let buffer_length = Buffer_data_output.length buffer in
  Alcotest.(check int) "The buffer length should be 29" 29 buffer_length


let test_write_a_string () =
  let strings = [
    "hello world";
    "";
  ] in
  let buffer = Buffer.create 10 in
  List.iter (fun s ->
    Buffer_data_output.write_string buffer s
  ) strings;
  let s = Buffer.contents buffer in
  let source = String_bytes_source.of_bytes s in
  List.iter (fun expected ->
    let result = String_data_input.read_string source in
    let test_message = Printf.sprintf "Retrieved string should be \"%s\"" expected in
    Alcotest.(check string) test_message expected result
  ) strings;
  let buffer_length = Buffer_data_output.length buffer in
  Alcotest.(check int) "The buffer length should be 13" 13 buffer_length

let tests = [
  "Read a vint", `Quick, test_write_a_vint;
  "Read a string", `Quick, test_write_a_string;
]