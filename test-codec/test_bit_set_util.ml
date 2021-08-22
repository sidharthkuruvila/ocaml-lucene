open Lucene_codec

let test_bit_is_set () =
  let bytes = [|'\xF0'; '\x00'; '\xF2'; '\xFF'|] in
  let get_byte_at i = int_of_char bytes.(i) in
  let result = Bit_set_util.is_bit_set ~get_byte_at 17 in
  Alcotest.(check bool) "Is the bit set?" result true


let test_bit_not_set () =
  let bytes = [|'\xFA'; '\x00'; '\xF2'; '\xFF'|] in
  let get_byte_at i = int_of_char bytes.(i) in
  let result = Bit_set_util.is_bit_set ~get_byte_at 18 in
  Alcotest.(check bool) "Is the bit set?" result false

let tests = [
  "is_bit_set should return true if set bit is found", `Quick, test_bit_is_set;
  "is_bit_set should return false if set bit is not found", `Quick, test_bit_not_set;
]