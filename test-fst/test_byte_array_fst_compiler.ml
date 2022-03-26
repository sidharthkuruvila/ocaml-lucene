open Lucene_fst
open Lucene_data_input_2

module Output_writer = String_output_writer.Make(Byte_array_fst_compiler.Data_output)

module Compiler = Byte_array_fst_compiler.Make(String_output)(Output_writer)

module P = Reverse_bytes_source.Make(String_bytes)
module Q = Data_input.Make(P)
module R = String_output_reader.Make(Q)
module S = Byte_array_fst_reader.Make(Q)(String_output)(R)
module T = Byte_array_fst_reader_utils.Make(S)

let test_compile_byte_array_fst () =
  let open Compiler in
  let arcs = [
    't', "act", "muh";
    'a', "ar", "guh";
    'c', "cat", "ignored";
  ] in
  let (buffer, first_state) = run (
    fold_left (fun target (ch, output, final_output) ->
      let transitions = [{State.ch; output; target}] in
      let state = { State.transitions; final_output = Some final_output } in
      compile_state state
    ) (compile_state { State.transitions = []; final_output = Some "s"}) arcs
  ) in
  Alcotest.(check int) "First state should be less than the size of the buffer" (Buffer.length buffer) (first_state + 1);
  let fst_bytes = Buffer.contents buffer in
  let di = P.of_bytes fst_bytes in
  let fst = S.create ~di ~start_node:first_state ~empty_output:"" in
  let path = T.fst_match_term ~fst "cat" in
  let result = T.make_output path in
  let expected = "cataracts" in
  Alcotest.(check string) "Expect result for input \"cat\" to be \"cataracts\"" expected result;
  let path = T.fst_match_term ~fst "ca" in
  let result = T.make_output path in
  let expected = "catarmuh" in
  Alcotest.(check string) "Expect result for input \"ca\" to be \"catarmuh\"" expected result;
  let path = T.fst_match_term ~fst "c" in
  let result = T.make_output path in
  let expected = "catguh" in
  Alcotest.(check string) "Expect result for input \"c\" to be \"catguh\"" expected result


let tests = [
  "Build a byte array fst from uncompiled states", `Quick, test_compile_byte_array_fst;
]