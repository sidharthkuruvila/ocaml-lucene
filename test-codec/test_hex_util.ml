open Lucene_codec

let test_hex_of_string () =
  let result = Hex_util.hex_of_string "abcdef" in
  Alcotest.(check string) "A string renders to hex" result "61 62 63 64 65 66 "


let tests = [
  "is_bit_set should return true if set bit is found", `Quick, test_hex_of_string;
]