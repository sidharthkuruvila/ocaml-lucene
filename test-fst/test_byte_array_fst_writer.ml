open Lucene_fst
open Lucene_data_output
open Lucene_data_input_2

module M = Data_output.Make(Buffer_bytes_writer)
module N = String_output_writer.Make(M)
module O = Byte_array_fst_writer.Make(M)(String_output)(N)

module P = Reverse_bytes_source.Make(String_bytes)
module Q = Data_input.Make(P)
module R = String_output_reader.Make(Q)
module S = Byte_array_fst_reader.Make(Q)(String_output)(R)
module T = Byte_array_fst_reader_utils.Make(S)

let test_build_a_byte_array_fst () =

  let arcs = [
    't', "act", "s";
    'a', "ar", "muh";
    'c', "cat", "guh"
  ] in
  let buffer = Buffer.create 10 in
  let start_target = List.fold_left (fun (current_target, previous_target) (label, output, final_output) ->
     let arc = Arc.({
       target = current_target;
       label = int_of_char label;
       output;
       final_output;
     }) in
     (O.write_node buffer previous_target [arc], current_target)) (-1, -1) arcs in
  let fst_bytes = Buffer.contents buffer in
  let di = P.of_bytes fst_bytes in
  let fst = S.create ~di ~start_node:(fst start_target) ~empty_output:"" in
  let path = T.fst_match_term ~fst "cat" in
  let result = T.make_output path in
  let expected = "cataracts" in
  Alcotest.(check string) "Expect result for input \"cat\" to be \"cataracts\"" expected result

let tests = [
  "Build a byte array fst", `Quick, test_build_a_byte_array_fst;
]