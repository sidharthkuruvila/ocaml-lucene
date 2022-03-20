module type Fst_compiler = sig
  type 'a t
  type state
  module Output: Output.S
  val compile_state: (state, Output.t) State.t -> state t
end

module Make(Fst: Fst.S) = struct
   open Fst
   type temporary_transition = {
     ch: char;
     output: Output.t;
     from_state: (Fst.state, Output.t) State.t;
   }

   let push_output (current_output, old_output, acc) temporary_state_transition =
     let existing_output = Output.add old_output temporary_state_transition.output in
     let common_prefix = Output.common current_output existing_output in
     let word_suffix = Output.subtract common_prefix existing_output in
     let remainder = Output.subtract common_prefix current_output in
     let from_state = temporary_state_transition.from_state in
     let from_state = State.update_transitions from_state ~f:(fun transition ->
            { transition with State.output = (Output.add old_output transition.State.output) }) in
     let from_state = State.update_final_output from_state ~f:(fun current_final_output -> Output.add old_output current_final_output) in
     let updated_transition = {temporary_state_transition with output = common_prefix; from_state } in
     (remainder, word_suffix, updated_transition::acc)

   let compile_temporary_state_transition temporary_state_transition compiled_next_state =
     let { ch; output; from_state } = temporary_state_transition in
     let from_state = State.set_transition from_state ch output compiled_next_state in
     compile_state from_state

  let update_common_state_transition common_state_transition new_char remaining_output old_output compiled_suffix_state =
    let { ch; output; from_state } = common_state_transition in
    let from_state = State.update_transitions from_state ~f:(fun transition ->
          { transition with State.output = (Output.add old_output transition.State.output) }) in
    let from_state = State.set_transition from_state ch (Output.add old_output output) compiled_suffix_state in
    let from_state = State.update_final_output from_state ~f:(fun current_final_output -> Output.add old_output current_final_output) in
    { ch = new_char; output = remaining_output; from_state }

  let make_word word output final_output =
    let n = String.length word in
    if n = 0 then []
    else
      let rec loop i =
        if i = n then
          []
        else
          let ch = String.get word i in
          let temporary_state_transition = {
            ch; output = Output.empty; from_state = State.init
          } in
          temporary_state_transition::loop (i+1) in
      let first_char = String.get word 0 in
      let from_state = State.init in
      let from_state =  Option.map (State.set_final_output from_state) final_output |> Option.value ~default:from_state in
      let first_temporary_state_transition = {
        ch = first_char; output = output; from_state
      } in
      let remaining_temporary_state_transitions = loop 1 in
      first_temporary_state_transition::remaining_temporary_state_transitions

  let add_word_when_prefix_short common_prefix common_state_transition new_char compilation_candidates remaining_next_input next_output =
    let (remaining_output, old_output, rev_updated_prefix) = List.fold_left push_output (next_output, Output.empty, []) common_prefix in
    let updated_prefix = List.rev rev_updated_prefix in
    let* empty_compiled_state = State.empty_final_state ~empty_output:Output.empty |> Fst.compile_state in
    let* compiled_suffix_state = Fst.fold_right compile_temporary_state_transition compilation_candidates (return empty_compiled_state) in
    let updated_common_state_transition = update_common_state_transition common_state_transition new_char remaining_output old_output compiled_suffix_state in
    let remaining_suffix = make_word remaining_next_input Output.empty None in
    return (List.concat [updated_prefix; [updated_common_state_transition]; remaining_suffix])

  let add_word_when_prefix_equal prefix last_state_transition remaining_next_input next_output =
    let (remaining_output, old_output, rev_updated_prefix) = List.fold_left push_output (next_output, Output.empty, []) prefix in
    let updated_prefix = List.rev rev_updated_prefix in
    let existing_output = Output.add old_output last_state_transition.output in
    let common_prefix = Output.common remaining_output existing_output in
    let remaining_output = Output.subtract common_prefix remaining_output in
    let remaining_old_output = Output.subtract common_prefix existing_output in
    let last_state_transition = { last_state_transition with
      output = common_prefix} in
    let remaining_suffix = make_word remaining_next_input remaining_output (Some remaining_old_output) in
    return (List.concat [updated_prefix; [last_state_transition]; remaining_suffix])

  let add_word current_word (next_input, next_output) =
    let current_word_letters = List.map (fun {ch; _} -> ch) current_word in
    let next_word_letters = String.to_seq next_input |> List.of_seq in
    let prefix_length = Lists.common_prefix_length current_word_letters next_word_letters ~compare:Char.compare in
    let (common_prefix, rest) = Lists.split_at_index current_word ~index:prefix_length in
    if List.length current_word = prefix_length then
      let remaining_next_input = String.sub next_input prefix_length (String.length next_input - prefix_length) in
      (* Our expectation is that there is at least one state transition in the current_word *)
      let [@warning "-8"] (prefix, [last_state_transition]) = Lists.split_at_index current_word ~index:(prefix_length - 1) in
      add_word_when_prefix_equal prefix last_state_transition remaining_next_input next_output
    else
      (* As the common prefix is shorter than current words rest should have at least one item *)
      let [@warning "-8"] common_state::compilation_candidates = rest in
      let remaining_next_input = String.sub next_input (prefix_length + 1) (String.length next_input - prefix_length - 1) in
      let new_char = String.get next_input prefix_length in
      add_word_when_prefix_short common_prefix common_state new_char compilation_candidates remaining_next_input next_output

  let create_minimal_transducer items =
    (* We can expect that there should be at least on item in items *)
    let [@warning "-8"] (first_word, first_output)::items = items in
    let first_word = make_word first_word first_output None in
    let* last_word = fold_left add_word (return first_word) items in
    let* empty_compiled_state = State.empty_final_state ~empty_output:Output.empty |> Fst.compile_state in
    Fst.fold_right compile_temporary_state_transition last_word (return empty_compiled_state)

end