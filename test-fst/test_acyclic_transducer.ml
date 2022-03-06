open Lucene_fst

let test_push_output () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let state_transition = {Builder.output = "prefix old suffix"; ch = 'c'; from_state = { transitions = []; final_output = Some " final output" } } in
  let current_output = "prefix new suffix" in
  let result = Builder.push_output (current_output, Output.empty, []) state_transition in
  let (updated_output, old_output, transitions) = result in
  let updated_state_transition = List.hd transitions in
  Alcotest.(check string) "The ouput should be the common prefix" (Output.to_string updated_state_transition.Builder.output) "prefix ";
  let from_state = updated_state_transition.Builder.from_state in
  Alcotest.(check string) "The old suffix should be pushed to the final output" (Output.to_string (State.get_final_output from_state ~default:Output.empty)) "old suffix final output";
  Alcotest.(check string) "The updated output should be the just the new suffix" (Output.to_string updated_output) "new suffix";
  Alcotest.(check string) "The updated output should be the just the new suffix" (Output.to_string old_output) "old suffix"

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
    let updated = Builder.update_common_state_transition common_state_transition new_char remaining_output Output.empty compiled_suffix_state in
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
  let final_output = (Some "final_output") in
  let word = Builder.make_word input output final_output in
  let result_input = List.map (fun t -> t.Builder.ch) word |> List.to_seq |> String.of_seq in
  let result_output = (List.hd word).output in
  let result_final_output = State.get_final_output (List.hd word).from_state ~default:"incorrect output" in
  Alcotest.(check string) "The letters in constructed word should be those of the input" result_input input;
  Alcotest.(check string) "The first transition of the word should containg the output" result_output output;
  Alcotest.(check string) "The final output should be set" "final_output" result_final_output

let test_make_word_empty_input_word () =
  let module Fst = Fst.Make(String_output) in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let input = "" in
  let output = "output_word" in
  let final_output = (Some "final_output") in
  let word = Builder.make_word input output final_output in
  Alcotest.(check int) "The length of the word should be 0" 0 (List.length word)

let test_add_word_when_common_prefix_shorter_than_current_word () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let current_word = [
    { Builder.output = "o1"; ch = 'c'; from_state = { transitions = []; final_output = None } };
    { Builder.output = "o2"; ch = 'a'; from_state = { transitions = []; final_output = None } };
    { Builder.output = "o3"; ch = 't'; from_state = { transitions = []; final_output = None } };
  ] in
  let next_word = "cars" in
  let next_output = "o1o3o4" in
  let run_fst code = ignore (Fst.run 'a' 'b' code) in
  run_fst (
    let* updated_current_word = Builder.add_word current_word (next_word, next_output) in
    let current_word_string = List.map (fun t -> t.Builder.ch) updated_current_word |> List.to_seq |> String.of_seq in
    let current_word_output = List.map (fun t -> t.Builder.output) updated_current_word  in
    Alcotest.(check string) "The updated word should match the next word string" next_word current_word_string;
    Alcotest.(check (list string)) "The updated word output should the next output" ["o1"; "o"; "3o4"; ""] current_word_output;
    let [@warning "-8"] [_; _; temporary_state_transition; _] = updated_current_word in
    let pushed_output = State.get_output temporary_state_transition.Builder.from_state 't' |> Option.get in
    Alcotest.(check string) "The suffix of the output for the char that diverged should be in its transition"
      "2o3" pushed_output;
    return ()
  )

let test_add_word_when_common_prefix_is_same_as_current_word () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let current_word = [
    { Builder.output = "o1"; ch = 'c'; from_state = { transitions = []; final_output = None } };
    { Builder.output = "o2"; ch = 'a'; from_state = { transitions = []; final_output = None } };
    { Builder.output = "o3"; ch = 't'; from_state = { transitions = []; final_output = None } };
  ] in
  let next_word = "cats" in
  let next_output = "o1o3o4" in
  let run_fst code = ignore (Fst.run 'a' 'b' code) in
  run_fst (
    let* updated_current_word = Builder.add_word current_word (next_word, next_output) in
    let current_word_string = List.map (fun t -> t.Builder.ch) updated_current_word |> List.to_seq |> String.of_seq in
    let current_word_output = List.map (fun t -> t.Builder.output) updated_current_word  in
    Alcotest.(check string) "The updated word should match the next word string" next_word current_word_string;
    Alcotest.(check (list string)) "The updated word output should the next output" ["o1"; "o"; "3o4"; ""] current_word_output;
    let [@warning "-8"] [_; _; _; temporary_state_transition] = updated_current_word in
    let pushed_output = State.get_final_output temporary_state_transition.Builder.from_state ~default:"output not found" in
    Alcotest.(check string) "The suffix of the output for the char that diverged should in the final output"
      "2o3" pushed_output;
    return ()
  )

let test_create_minimal_transducer test_name items =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let items = items |> List.sort (fun (a,_) (b, _) -> String.compare a b) in
  ignore (
     Fst.run 'a' 'z' (
     let* start_state = Builder.create_minimal_transducer items in
     let* _ = Fst.print_transducer start_state "out.dot" in
     let* results = Fst.fold_left (fun acc (i, o) -> let* res = Fst.accept i start_state in return ((i, res, o) :: acc)) (return []) items in
     List.iter (fun (i, res, o) ->
       let contained = Fst.Output_set.mem o res in
       Alcotest.(check bool) (Printf.sprintf "%s: Expected %s for input %s got %s" test_name o i (Fst.string_of_output_set res)) true contained) results;
     return ()))

let tests = [
  "Different outputs", ["ca", "bat"; "cc", "bar"];
  "More words", [
      "ca", "bat";
      "cat", "bat";
      "car", "bat";
      "co", "bat";
      "dog", "bar"];
  (*Not supported right now *)
  (*"Duplicate words ", [
        "ca", "bat";
        "cat", "bat";
        "cat", "bar";
        "car", "bat";
        "co", "bat";
        "dog", "bar"];*)
]

let test_create_minimal_transducers () =
  List.iter (fun (test_name, items) -> test_create_minimal_transducer test_name items) tests

let tests = [
  "Push the output through a temporary transition", `Quick, test_push_output;
  "Compile temporary state transitions", `Quick, test_compile_temporary_state_transition;
  "Update the common state transition", `Quick, test_update_common_state_transition;
  "Convert the input into a list of temporary state transitions", `Quick, test_make_word;
  "Converting an empty list should return an empty list", `Quick, test_make_word_empty_input_word;
  "Add a word when the common prefix is shorter than the current word", `Quick, test_add_word_when_common_prefix_shorter_than_current_word;
  "Add a word when the common prefix is the same as the current word", `Quick, test_add_word_when_common_prefix_is_same_as_current_word;
  "Construct a minimal transducer", `Quick, test_create_minimal_transducers;
]