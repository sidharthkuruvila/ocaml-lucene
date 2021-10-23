open Lucene_bit_packing
open Lucene_data_input

let test_encoding_decoding_positive_ints () =
  let module Decode = Positive_ints.Decode(String_data_input) in
  let module Encode = Positive_ints.Encode(Buffer_data_output) in
  let inputs = [
  ] in
  ()

let tests = [
  "encoded positive ints should be decodable", `Quick, test_encoding_decoding_positive_ints;
]