open Lucene_fst

let test_push_output () =
 let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let state_transition = {Builder.output = "prefix old suffix"; ch = 'c'; from_state = { transitions = []; final_output = Some " final output" } } in
  let current_output = "prefix new suffix" in
  let result = Builder.push_output (current_output, []) state_transition in
  let (updated_output, transitions) = result in
  let updated_state_transition = List.hd transitions in
  Alcotest.(check string) "The ouput should be the common prefix" (Output.to_string updated_state_transition.Builder.output) "prefix ";
  let from_state = updated_state_transition.Builder.from_state in
  Alcotest.(check string) "The old suffix should be pushed to the final output" (Output.to_string (State.get_final_output from_state ~default:Output.empty)) "old suffix final output";
  Alcotest.(check string) "The updated output should be the just the new suffix" (Output.to_string updated_output) "new suffix"


let tests = [
  "Push the output through a temporary transition", `Quick, test_push_output
]