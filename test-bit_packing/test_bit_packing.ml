open Lucene_bit_packing
open Lucene_data_input
open Lucene_utils

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
   0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111]; List_utils.fill 120 0 ] , 11]
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
      List_utils.fill 120 0], 2;
     List.concat [[0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111;
     0b11111111111; 0b11111111111; 0b11111111111; 0b11111111111]; List_utils.fill 120 0 ] , 11
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
