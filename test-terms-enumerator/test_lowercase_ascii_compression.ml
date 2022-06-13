open Lucene_data_input_2
open Lucene_data_output
open Lucene_terms_enumerator

let strings = [
  "This is a simple compressible string. It contains a number of capitalized letters."
]

module Reader = Lowercase_ascii_compression.Make_reader(String_data_input)
module Writer = Lowercase_ascii_compression.Make_writer(Buffer_data_output)


let test_ascii_compression () =
  List.iter (fun input_string ->
    let buf = Buffer.create 0 in
    Writer.compress buf input_string;
    let compressed = Buffer.contents buf in
    let data_input = String_data_input.of_bytes compressed in
    let uncompressed = Reader.decompress data_input (String.length input_string) in
    Alcotest.(check string) "Uncompressed string should be same as input" input_string uncompressed;
    Printf.printf "compressed: %s\n" compressed
  ) strings

let tests = [
  "Compress using lowercase ascii compression", `Quick, test_ascii_compression;
]