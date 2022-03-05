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
       let state_transitions = [
         { Builder.output = "output1"; ch = 'c'; from_state = { transitions = []; final_output = None } };
         { Builder.output = "output2"; ch = 'a'; from_state = { transitions = []; final_output = None } };
       ] in
       let* compiled_state = Fst.fold_right Builder.compile_temporary_state_transition state_transitions (return empty_compiled_state) in
       let* result_set = Fst.accept "ca" compiled_state in
       let result = Fst.Output_set.to_seq result_set
         |> List.of_seq
         |> List.map Output.to_string in
       Alcotest.(check (list string)) "The string should match and return the correct output" ["output1output2"] result;
       return ()
  ))

let test_update_common_state_transition () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let common_state_transition = { Builder.output = "output1"; ch = 'c'; from_state = { transitions = []; final_output = None } }in
  let new_char = 'b' in
  let remaining_output = "output2" in
  let run_fst code = ignore (Fst.run 'a' 'b' code) in
  run_fst (
    let* compiled_suffix_state = State.empty_final_state ~empty_output:Output.empty |> Fst.compile_state in
    let updated = Builder.update_common_state_transition common_state_transition new_char remaining_output compiled_suffix_state in
    Alcotest.(check char) "The char in the updated state should be the new char" updated.Builder.ch new_char;
    Alcotest.(check string) "The output in the udpated state should be the remaining_utput" updated.Builder.output remaining_output;
    Alcotest.(check (option string)) "The original char and output should be pushed into the from_state"
      (State.get_output updated.Builder.from_state 'c') (Some "output1");
    return ()
  )

let test_make_word () =
  let module Fst = Fst.Make(String_output) in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let input = "input_word" in
  let output = "output_word" in
  let word = Builder.make_word input output in
  let result_input = List.map (fun t -> t.Builder.ch) word |> List.to_seq |> String.of_seq in
  let result_output = (List.hd word).output in
  Alcotest.(check string) "The letters in constructed word should be those of the input" result_input input;
  Alcotest.(check string) "The first transition of the word should containg the output" result_output output


let tests = [
  "Push the output through a temporary transition", `Quick, test_push_output;
  "Compile temporary state transitions", `Quick, test_compile_temporary_state_transition;
  "Update the common state transition", `Quick, test_update_common_state_transition;
  "Convert the input into a list of temporary state transitions", `Quick, test_make_word;
]