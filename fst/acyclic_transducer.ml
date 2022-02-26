(**
 This is a generic implementation for building the prefix tree used to build a look up table into lucene's index.

 It is based on Direct Construction of Minimal Acyclic Transducers https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.24.3698

*)

let common_prefix_length s1 s2 =
 let n = Int.min (String.length s1) (String.length s2) in
 let rec loop i =
   if i = n || not (Char.equal (String.get s1 i) (String.get s2 i)) then i
   else loop (i + 1) in
 loop 0

module Make (Fst: Fst.S) = struct
open Fst

let rec make_n_states n =
  if n = 0 then return []
  else
    let* state = create_state in
    let* list = make_n_states (n - 1) in
    return (state :: list)

let minimize_suffix temp_states word prefix_length =
  let rec loop i temp_states=
    if i = prefix_length - 1 then
      (return temp_states)
    else begin
      match temp_states with
      | (next_state::prev_state::rest) ->
        let ch = String.get word i in
        let* min_state = find_minimized next_state in
        let* _ = set_transition prev_state ch min_state in
        (loop (i - 1) (prev_state::rest))
      | _ -> failwith "minimize_suffix: Should not come here" end in
 loop (String.length word - 1) temp_states

let initialize_tail_states temp_states word prefix_length =
  let rec loop i temp_states =
    if i = String.length word then
      return temp_states
    else
      match temp_states with
      | prev_state::_ ->
        let ch = String.get word i in
        let* next_state = create_state in
        let* _ = set_transition prev_state ch next_state in
        loop (i + 1) (next_state::temp_states)
      | _ -> failwith "initialize_tail_states: Should not come here" in

  loop prefix_length temp_states

let find_max_width items = List.fold_left (fun current_max (word, _) -> Int.max current_max (String.length word)) 0 items

let rec split_at l index =
  match index, l with
  | 0, l -> ([], l)
  | n, x::rest ->
      let (l1, l2) = split_at rest (n - 1)  in
      (x::l1, l2)
  | _, [] -> ([], [])

let push_output current_word (temp_states: Fst.state list) i current_output =
  let states = List.rev temp_states in
  let rec loop states i current_output =
   if i = String.length current_word then
     return current_output
   else
     match states with
     | [] | [_] -> failwith "states list is too short"
     | current_state :: next_state :: rest ->
     let current_ch = String.get current_word i in
     let* existing_output = output current_state current_ch >>| Option.value ~default:current_output in
     let common_prefix = Output.common existing_output current_output in
     let word_suffix = Output.subtract common_prefix existing_output in
     let* _  = set_output current_state current_ch common_prefix in
     let* next_outputs = outputs next_state in
     let* _ = fold_left (fun _ (ch, _) ->
       let* old_output = output_str next_state ch in
       let* _ = set_output next_state ch (Output.add word_suffix old_output) in
       return ()
     ) (return ()) next_outputs in
     let* _ = cond (final next_state) ~if_true:(fun () ->
       let* strings = state_output next_state in
       let updated_strings = Output_set.map (fun s -> Output.add word_suffix s) strings in
       set_state_output next_state updated_strings
     ) ~if_false:(fun () -> return ()) in
     let current_output = Output.subtract common_prefix current_output in
     loop (next_state::rest) (i + 1) current_output in
   loop states i current_output

let add_to_transducer ((temp_states: Fst.state list), (previous_word: string)) (current_word, current_output : (string * Output.t)) =
  let prefix_length = common_prefix_length previous_word current_word in
  assert (prefix_length >= 0);
  (* Minimise the suffix of the previous word *)
   let* temp_states = minimize_suffix temp_states previous_word prefix_length in
   assert (List.length temp_states > 0);
   (* Initialize tail states for current word *)
   let* temp_states = initialize_tail_states temp_states current_word prefix_length in
   let last_state = List.hd temp_states in
   (* Set last char transition of current word as final *)
   let* _  = if not (String.equal current_word previous_word) then
       let* _ = set_final last_state true in
       set_state_output last_state (Output_set.singleton Output.empty)
     else return () in
     let* current_output = push_output current_word temp_states 0 current_output in
     if Output.compare current_output Output.empty <> 0 then Printf.printf "Remaining output = %s\n" (Output.to_string current_output);
     let* _ = if String.equal current_word previous_word then
       let* sos = state_output last_state in
       Printf.printf "current_word: %s, current_output: %s, sos: %s\n" current_word (Output.to_string current_output)
        (string_of_output_set sos);
       set_state_output last_state (Output_set.add current_output sos)
     else
       return () in
     return (temp_states, current_word)

let merge_final_word temp_states current_word i =
  let rec loop i temp_states =
     if i < 0 then
       return ()
     else
       match temp_states with
       | next_state::current_state::rest ->
         let* min_state = find_minimized next_state in
         let* _ = set_transition current_state (String.get current_word i) min_state in
         loop (i-1) (current_state::rest)
       | _ -> failwith "does not match" in
  loop i temp_states

let create_minimal_transducer items =
  (* The previous string in the loop, initialized to an empty string for the first run *)
  let previous_word = "" in
  (* An array containing temprary states for building the fst *)
  let* first_state = create_state in
  let temp_states = [first_state] in
  let* (temp_states, current_word) = fold_left add_to_transducer (return (temp_states, previous_word)) items in
  let* _ = merge_final_word temp_states current_word (String.length current_word - 1) in
  find_minimized first_state
end