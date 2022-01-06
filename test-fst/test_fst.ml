open Lucene_fst
open Fst

let test_final () =
  ignore (Fst.run 'a' 'z' (
  let* state = Fst.create_state in
  let* is_final = Fst.final state in
  Alcotest.(check bool) "Final state should be false" is_final false;
  let* _ = Fst.set_final state true in
  let* is_final = Fst.final state in
  Alcotest.(check bool) "Final state should be true" is_final true;
  let* _ = Fst.set_final state false in
  let* is_final = Fst.final state in
  Alcotest.(check bool) "Final state should be false" is_final false;
  return ()))

let test_transition () =
  ignore (Fst.run 'a' 'z' (
  let* state1 = Fst.create_state in
  let* state2 = Fst.create_state in
  let* _ = Fst.set_transition state1 'a' state2 in
  let* next_state = Fst.transition state1 'a' >>| Option.map Fst.state_to_int in
  Alcotest.(check (option int)) "transition should return the transition state"  next_state @@ Some (state_to_int state2);
  return ()))

let test_state_output () =
  ignore (Fst.run 'a' 'z' (
  let* state = Fst.create_state in
  let* _ = Fst.set_state_output state (Fst.Output_set.singleton "abc") in
  let* res = Fst.state_output state >>| Fst.Output_set.to_seq >>| List.of_seq in
  Alcotest.(check (list string)) "state_output should return the set of outputs"  res @@ ["abc"];
  return ()))
(*
let test_compare () =
  ignore (Fst.run 'a' 'z' (
  let* states_seq = Fst.all [
    Fst.create_state;
    Fst.create_state;
    Fst.create_state;
    Fst.create_state;
  ] in
  let states = states_seq |> Array.of_list in
  let* _ = Fst.fold_left (fun _ a -> a) (Fst.return ()) [
    Fst.set_transition states.(0) 'a' states.(3);
    Fst.set_transition states.(1) 'a' states.(3);
  ] in
  let* res = Fst.compare_states states.(0) states.(1) in
  Alcotest.(check int) "like items are equal" res 0;
  return ()))
*)
let tests = [
  "final and set_final should return and update the final flag for a state", `Quick, test_final;
  "Setting a transition should allow it to be returned", `Quick, test_transition;
  "Setting a state_output should allow it to be returned", `Quick, test_state_output;
(*  "States should be comparable", `Quick, test_compare;*)
]