open Lucene_data_input_2
open Lucene_terms_enumerator

module Reader = Lz4_compression.Make_reader(String_data_input)

let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let test_decompress_using_lz4_compression () =
  let s = read_whole_file "data/lz4-compressed.bytes" in
  let di = String_data_input.of_bytes s in
  let result = Reader.decompress di 245 in
  let expected = "1-04-011-05-011-08-011-10-011-12-012-02-012-03-012-04-012-06-012-08-012-09-012-10-013-01-013-02-013-03-013-04-013-05-013-06-013-07-013-08-013-09-013-10-013-11-013-12-014-01-014-02-014-03-014-04-014-05-014-06-014-07-014-08-014-09-014-10-014-11-01" in
  Alcotest.(check string) "Decompress an input string" expected result

let tests = [
  "Decompress using lz4 compression", `Quick, test_decompress_using_lz4_compression;
]