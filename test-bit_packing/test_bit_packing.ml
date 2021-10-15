open Lucene_bit_packing

let test_decoding_encoded_lists () =
  let inputs = [
   [2;0;0;0;3;3;3;3], 2;
   [0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111;
   0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111], 11
  ] in
  List.iter (fun (l, bit_count) ->
    let encoded = Bit_packing.encode l bit_count in
    let decoded = Bit_packing.decode encoded bit_count in
    Alcotest.(check (list int)) "Decoding an encoded list" decoded l
  ) inputs



let tests = [
  "encoded byte lists should be decodeable", `Quick, test_decoding_encoded_lists;
]
