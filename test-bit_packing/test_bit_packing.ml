open Lucene_bit_packing
open Lucene_data_input

let binary64 n =
  let open Int64 in
  let bytes = Bytes.make 64 '0' in
  let rec loop i =
    let c = if logand (shift_left 1L i) n = 0L then
      '0'
    else
      '1' in
    Bytes.set bytes (63 - i) c;
    if i < 63 then loop (i + 1) in
  loop 0;
  Bytes.to_string bytes

let rec fill n v =
  if n = 0 then []
  else v::(fill (n - 1) v)

let test_patching_unpatching_lists () =
  let inputs = [
   [2;0;0;0;3;3;3;3;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;
    0;0;0;0;0;0;0;0;], 2;
   List.concat [[0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111;
   0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111]; fill 120 0 ] , 11]
  in
  List.iter (fun (l, bit_count) ->
    let encoded = Bit_packing.pack l bit_count in
    let decoded = Bit_packing.unpack encoded bit_count in
    Alcotest.(check (list int)) "Decoding an encoded list" l decoded
  ) inputs

let test_encoding_decoding_lists () =
  let module Decode = Bit_packing.Decode(String_data_input) in
  let module Encode = Bit_packing.Encode(Buffer_data_output) in
    let inputs = [
     List.concat [[2;0;0;0;3;3;3;3;];
      fill 120 0], 2;
     List.concat [[0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111;
     0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111]; fill 120 0 ] , 11
     ]
    in
  List.iter (fun (l, bit_count) ->
    let buffer = Buffer.create 128 in
    let dout = Buffer_data_output.from_buffer buffer in
    Encode.encode l bit_count dout;
    let encoded = Buffer.to_bytes buffer |> Bytes.to_string in
    Printf.printf "Encoded:\n %d\n\n"(String.length encoded);
    let din = String_data_input.from_string encoded in
    let decoded = Decode.decode bit_count din in
    Alcotest.(check (list int)) "Decoding an encoded list" l decoded
  ) inputs


let tests = [
  "encoded byte lists should be patchable", `Quick, test_patching_unpatching_lists;
  "encoded byte lists should be decodeable", `Quick, test_encoding_decoding_lists;
]
