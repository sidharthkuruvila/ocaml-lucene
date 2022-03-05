module type Fst_compiler = sig
  type 'a t
  type state
  module Output: Output.S
  val compile_state: (state, Output.t) State.t -> state t
end

module Make(Fst: Fst_compiler) = struct
   open Fst
   type temporary_transition = {
     ch: char;
     output: Output.t;
     from_state: (Fst.state, Output.t) State.t;
   }

   let push_output (current_output, acc) temporary_state_transition =
     let existing_output = temporary_state_transition.output in
     let common_prefix = Output.common current_output temporary_state_transition.output in
     let word_suffix = Output.subtract common_prefix existing_output in
     let remainder = Output.subtract common_prefix current_output in
     let from_state = temporary_state_transition.from_state in
     let from_state = State.update_transitions from_state ~f:(fun transition ->
      { transition with State.output = (Output.add word_suffix transition.State.output) }) in
     let from_state = State.update_final_output from_state ~f:(fun current_final_output -> Output.add word_suffix current_final_output) in
     let updated_transition = {temporary_state_transition with output = common_prefix; from_state } in
     (remainder, updated_transition::acc)

   let compile_temporary_state_transition temporary_state_transition compiled_next_state =
     let { ch; output; from_state } = temporary_state_transition in
     let from_state = State.set_transition from_state ch output compiled_next_state in
     compile_state from_state
end