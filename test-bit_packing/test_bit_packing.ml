open Lucene_bit_packing


let rec fill n v =
  if n = 0 then []
  else v::(fill (n - 1) v)

let test_decoding_encoded_lists () =
  let inputs = [
   [2;0;0;0;3;3;3;3;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;], 2;
(*   [2;0;0;0;3;3;3;3], 2;*)
   List.concat [[0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111;
   0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111]; fill 120 0 ] , 11]
  in
  List.iter (fun (l, bit_count) ->
    let encoded = Bit_packing.pack l bit_count in
    let decoded = Bit_packing.unpack encoded bit_count in
    Alcotest.(check (list int)) "Decoding an encoded list" l decoded
  ) inputs



let tests = [
  "encoded byte lists should be decodeable", `Quick, test_decoding_encoded_lists;
]
