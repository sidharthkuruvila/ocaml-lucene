open Lucene_bit_packing
open Lucene_data_input

let test_encode_positive_ints () =
  let module M = Positive_ints.Make(Buffer_data_input) in
  ()