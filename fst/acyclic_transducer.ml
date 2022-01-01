
let common_prefix_length s1 s2 =
 let n = Int.min (String.length s1) (String.length s2) in
 let rec loop i =
   if i = n || not (Char.equal (String.get s1 i) (String.get s2 i)) then i
   else loop (i + 1) in
 loop 0

let longest_common_prefix s1 s2 =
   let l = common_prefix_length s1 s2 in
   String.sub s1 0 l


(* Return what is left from the second string
once the common prefix is removed *)
let remainder s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s2 l (String.length s2 - l)

let concat s1 s2 = s1 ^ s2

module Make (Fst_builder: Fst.S) = struct
open Fst
let find_minimized state =
  let* r = member state in
  match r with
  | None ->
  let* copy = copy_state state in
  let* _ = insert copy in return copy
  | Some state -> return state

let rec make_n_states n =
  if n = 0 then return []
  else
    let* state = create_state in
    let* list = make_n_states (n - 1) in
    return (state :: list)


let minimize_suffix temp_states word prefix_length =
  let rec loop i =
    if i = prefix_length - 1 then
      (return ())
    else
      let ch = String.get word i in
      let next_state = temp_states.(i+1) in
      let prev_state = temp_states.(i) in
      let* min_state = find_minimized next_state in
      let* _ = set_transition prev_state ch min_state in
      (loop (i - 1)) in
 loop (String.length word - 1)

let initialize_tail_states temp_states word prefix_length =
  let rec loop i =
    if i = String.length word then
      return ()
    else
      let ch = String.get word i in
      let prev_state = temp_states.(i) in
      let next_state = temp_states.(i+1) in
      let* _ = clear_state next_state in
      let* _ = set_transition prev_state ch next_state in
      loop (i + 1) in
  loop prefix_length

let create_minimal_transducer first_char last_char items =
  let max_width = List.fold_left (fun current_max (word, _) -> Int.max current_max (String.length word)) 0 items in
  run first_char last_char (
  let previous_word = "" in
  let* temp_states =
       let* list = make_n_states (max_width + 1) in
       return (Array.of_list list) in
  let add_to_transducer (previous_word: string) (current_word, current_output : (string * string)) : string t =
    let prefix_length = common_prefix_length previous_word current_word in
    (* Minimise the suffix of the previous word *)
     let* _ = minimize_suffix temp_states previous_word prefix_length in
     (* Initialize tail states for current word *)
     let* _ = initialize_tail_states temp_states current_word prefix_length in
     (* Set last char transition of current word as final *)
     let* _  = if not (String.equal current_word previous_word) then
         let* _ = set_final temp_states.(String.length current_word) true in
         set_state_output temp_states.(String.length current_word) (String_set.singleton "")
       else return () in
     let rec loop i current_output = begin
       if i = String.length current_word then
         return ()
       else
         let* _ = return (Printf.printf "Before\n") in
         let* _ = debug in
         let current_ch = String.get current_word i in
         let current_state = temp_states.(i) in
         let* existing_output = output temp_states.(i) current_ch in
         let existing_output = Option.value existing_output ~default:current_output in
         let common_prefix = longest_common_prefix existing_output current_output in
         let word_suffix = remainder common_prefix existing_output in
         let* _ = return (Printf.printf "common prefix: %s, word suffix = %s\n" common_prefix word_suffix) in
         let* _  = set_output temp_states.(i) current_ch common_prefix in
         let* current_transitions = transitions current_state in
         let* _ = fold_left (fun _ (ch, next_state) ->
           let* old_output = output_str next_state ch in
           set_output next_state ch (concat word_suffix old_output)
         ) (return ()) current_transitions in
         let* _ = cond (final temp_states.(i)) ~if_true:(fun () ->
           let* strings = state_output temp_states.(i) in
           let updated_strings = String_set.map (fun s -> concat word_suffix s) strings in
           set_state_output temp_states.(i) updated_strings
         ) ~if_false:(fun () -> return ()) in
         let current_output = remainder common_prefix current_output in
        let* _ = return (Printf.printf "After\n") in
        let* _ = debug in
        let* _ = return (Printf.printf "------------------------\n\n") in
         loop (i + 1) current_output end in
       let* _  = loop 0 current_output in
       return current_word in
  let* current_word = fold_left add_to_transducer (return previous_word) items in
  let rec loop i =
     if i = 0 then
       return ()
     else
       let* min_state = find_minimized temp_states.(i+1) in
       let* _ = set_transition temp_states.(i) (String.get current_word i) min_state in
       loop (i-1) in
   let* _ = loop (String.length current_word - 1) in
   let* start_state = find_minimized temp_states.(0) in
   let* _ = debug in
   let* _ = return (Printf.printf "Final transducer\n") in
   Fst.print_transducer start_state "out.dot")

end