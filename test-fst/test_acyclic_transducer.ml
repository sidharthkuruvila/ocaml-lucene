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

let test_compile_temporary_state_transition () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  ignore (
       Fst.run 'a' 'z' (
       let* empty_compiled_state = State.empty_final_state ~empty_output:Output.empty |> Fst.compile_state in
       let state_transition_1 = { Builder.output = "output1"; ch = 'c'; from_state = { transitions = []; final_output = None } } in
       let state_transition_2 = { Builder.output = "output2"; ch = 'a'; from_state = { transitions = []; final_output = None } } in
       let* compiled_state_2 = Builder.compile_temporary_state_transition state_transition_2 empty_compiled_state in
       let* compiled_state_1 = Builder.compile_temporary_state_transition state_transition_1 compiled_state_2 in
       let* result_set = Fst.accept "ca" compiled_state_1 in
       let result = Fst.Output_set.to_seq result_set
         |> List.of_seq
         |> List.map Output.to_string in
       Alcotest.(check (list string)) "The string should match and return the correct output" ["output1output2"] result;
       return ()
  ))

let tests = [
  "Push the output through a temporary transition", `Quick, test_push_output;
  "Compile temporary state transitions", `Quick, test_compile_temporary_state_transition;
]