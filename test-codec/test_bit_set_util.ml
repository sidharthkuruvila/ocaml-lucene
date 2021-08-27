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


let test_count_bits () =
  let n = 0x1F in
  let result = Bit_set_util.count_bits n in
  Alcotest.(check int) "Are the bits counted?" result 5


let test_count_bits_upto () =
  let bytes = [|'\xF0'; '\x00'; '\xF3'; '\xFF'|] in
  let get_byte_at i = int_of_char bytes.(i) in
  let result = Bit_set_util.count_bits_upto ~get_byte_at 18 in
  Alcotest.(check int) "Is the count 16?" result 6

let tests = [
  "is_bit_set should return true if set bit is found", `Quick, test_bit_is_set;
  "is_bit_set should return false if set bit is not found", `Quick, test_bit_not_set;
  "test_count_bits should return the number of set bits", `Quick, test_count_bits;
  "test_count_bits_upto should return the number of set bits in butset", `Quick, test_count_bits_upto;
]