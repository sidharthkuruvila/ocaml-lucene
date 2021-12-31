module Int_set = Set.Make(Int)

module Int_map = Map.Make(Int)

module Char_map = Map.Make(Char)

module String_set = Set.Make(String)

module Transition = struct
  type t = (int * char) [@@deriving ord]
end

type t = {
  next_state: int;
  first_char: char;
  last_char: char;
  final_states: Int_set.t;
  transitions:  int Char_map.t Int_map.t;
  state_outputs: String_set.t Int_map.t;
  outputs: string Char_map.t Int_map.t;
  dictionary: int list

}

let make first_char last_char =
  {
    next_state = 0;
    first_char;
    last_char;
    final_states = Int_set.empty;
    transitions = Int_map.empty;
    state_outputs = Int_map.empty;
    outputs = Int_map.empty;
    dictionary = [];
  }

type 'a m = t -> ('a * t)

let show_dictionary { dictionary; _} = (String.concat ", " (List.map (fun n -> Printf.sprintf "%d" n) dictionary))

let bind m f t = let (a, t) = (m t) in f a t

let return a t = (a, t)

let value (a, _) = a
let st (_, state) = state

let (let*) = bind

(*
Create a new state in the transducer.
This is done by adding empty items in the transitions, state_outputs and outputs maps
The empty maps make it easier to update operations by removing the need for presence
checks.
*)
let create_state transducer =
  let {
    next_state;
    transitions;
    state_outputs;
    outputs;
    _
  } = transducer in
  (next_state, {
    transducer with
    next_state = next_state + 1;
    transitions = Int_map.add next_state Char_map.empty transitions;
    state_outputs = Int_map.add next_state String_set.empty state_outputs;
    outputs = Int_map.add next_state Char_map.empty outputs;
  })

let final state transducer =
  (Int_set.mem state transducer.final_states, transducer)

let set_final state flag transducer =
  let final_states = if flag then
    Int_set.add state transducer.final_states
  else
    Int_set.remove state transducer.final_states in
  ((), { transducer with final_states })

let transition state char transducer =
  let state_transitions = Int_map.find state transducer.transitions in
  (Char_map.find_opt char state_transitions, transducer)

let transitions state transducer =
  (Int_map.find state transducer.transitions |> Char_map.to_seq |> List.of_seq, transducer)

let set_transition state char new_state transducer =
  let transitions = transducer.transitions in
  let state_transitions = Int_map.find state transitions in
  let updated_state_transitions = Char_map.add char new_state state_transitions in
  let transitions = Int_map.add state updated_state_transitions transitions in
  ((), { transducer with transitions })

let state_output state transducer =
  (Int_map.find state transducer.state_outputs, transducer)

let set_state_output state outputs transducer =
  let state_outputs = Int_map.add state outputs transducer.state_outputs in
  ((), { transducer with state_outputs })

let output state char transducer =
  let state_outputs = Int_map.find_opt state transducer.outputs in
    (Option.bind state_outputs (fun state_outputs -> Char_map.find_opt char state_outputs), transducer)

let output_str state char =
  let* o = output state char in
  return (match o with
  | None -> ""
  | Some s -> s
  )


let set_output state char output transducer =
  let outputs = transducer.outputs in
  let outputs = match Int_map.find_opt state outputs with
  | None -> Int_map.add state (Char_map.singleton char output) outputs
  | Some state_transitions ->
    let updated_state_transitions = Char_map.add char output state_transitions in
    Int_map.add state updated_state_transitions outputs in
  ((), { transducer with outputs })

let copy_state state (transducer: t) =
  let {
    next_state;
    final_states;
    transitions;
    state_outputs;
    outputs;
    _
  } = transducer in
  let final_states = if final state transducer |> value then Int_set.add next_state final_states else final_states in
  let transitions = Int_map.add next_state (Int_map.find state transitions) transitions in
  let state_outputs = Int_map.add next_state (Int_map.find state state_outputs) state_outputs in
  let outputs = Int_map.add next_state (Int_map.find state outputs) outputs in
  (next_state, { transducer with next_state = next_state + 1; transitions; state_outputs; outputs; final_states })

let clear_state state transducer=
  let {
    final_states;
    transitions;
    outputs;
    _
  } = transducer in
  ((), {
    transducer with
    final_states = Int_set.remove state final_states;
    transitions = Int_map.add state Char_map.empty transitions;
    (* TODO Should the outputs be removed, I can't see the value of keeping them
       without the transitions *)
    outputs = Int_map.add state Char_map.empty outputs;
  })

(* TODO Should we pass separate copies of the transducer
   for each state

   TODO This implementation is going to be very slow. It would be be better to move to a hashtable
   based impementation as lucene has.
   *)
let rec compare_states state1 state2 transducer =
  (* Use comparision to check if both states are final states or not *)
  let compare_final_states state1 state2 =
    let state1_final = final state1 transducer |> value |> Bool.to_int in
    let state2_final = final state2 transducer |> value |> Bool.to_int in
    state1_final - state2_final in
  let final_cmp = compare_final_states state1 state2 in
  if final_cmp <> 0 then
    final_cmp
  else
    let state1_transitions = Int_map.find state1 transducer.transitions in
    let state2_transitions = Int_map.find state2 transducer.transitions in
    let state1_labels = state1_transitions |> Char_map.bindings |> List.map fst in
    let state2_labels = state2_transitions |> Char_map.bindings |> List.map fst in
    (* Compare number of transitions *)
    (* Compare each each transition label *)
    let labels_cmp = List.compare Char.compare state1_labels state2_labels in
    if labels_cmp <> 0 then
      labels_cmp
    else
      (* At this point we know that the labels are the same so lets use one of the lists *)
      let labels = state1_labels in
      (* Compare the output of each transition *)
      let state1_outputs = Int_map.find state1 transducer.outputs in
      let state2_outputs = Int_map.find state2 transducer.outputs in
      let outputs_1 = List.map (fun label -> Char_map.find label state1_outputs) labels in
      let outputs_2 = List.map (fun label -> Char_map.find label state2_outputs) labels in
      let outputs_cmp = List.compare String.compare outputs_1 outputs_2 in
      if outputs_cmp <> 0 then
        outputs_cmp
      else
        (* Compare the next state of each transition *)
        let states_1 = List.map (fun label -> Char_map.find label state1_transitions) labels in
        let states_2 = List.map (fun label -> Char_map.find label state2_transitions) labels in
        List.compare (fun s1 s2 ->compare_states s1 s2 transducer) states_1 states_2


let member state (transducer: t) =
  (List.find_opt (fun stored_state -> compare_states stored_state state transducer = 0 ) transducer.dictionary, transducer)

let insert state transducer =
   ((), { transducer with dictionary = state::transducer.dictionary })

let print_transducer transducer state filename =
  let oc = open_out filename in
  let state_id state = Printf.sprintf "state_%d" state in
  Printf.fprintf oc "digraph {";
  let rec loop state =
    let transitions = Int_map.find state transducer.transitions  in
    let outputs = Int_map.find state transducer.outputs in
    let labels = transitions |> Char_map.bindings |> List.map fst in
    let node_shape = if final state transducer |> value then "doublecircle" else "circle" in
    Printf.fprintf oc "%s [label = \"%d\" shape = \"%s\"]\n" (state_id state) state node_shape;
    List.iter (fun label ->
        let to_state = Char_map.find label transitions in
        let arc_output = Char_map.find label outputs in
        Printf.fprintf oc "%s -> %s [label = \"%c/%s\"]\n" (state_id state) (state_id to_state) label arc_output;
        loop to_state
        ) labels in
  loop state;
  Printf.fprintf oc "}";
  close_out oc


let find_minimized state =
  let* r = member state in
  match r with
  | None ->
  let* copy = copy_state state in
  let* _ = insert copy in return copy
  | Some state -> return state

let common_prefix_length s1 s2 =
 let n = Int.min (String.length s1) (String.length s2) in
 let rec loop i =
   if i = n || not (Char.equal (String.get s1 i) (String.get s2 i)) then i
   else loop (i + 1) in
 loop 0


let rec make_n_states n =
  if n = 0 then return []
  else
    let* state = create_state in
    let* list = make_n_states (n - 1) in
    return (state :: list)

let longest_common_prefix s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s1 0 l

(* Return what is left from the second string
once the common prefix is removed *)
let remainder s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s2 l (String.length s2 - l)

let concat s1 s2 = s1 ^ s2

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
  let transducer = make first_char last_char in
  let previous_word = "" in
  let (temp_states, transducer) =
       let (list, transducer) = make_n_states (max_width + 1) transducer in
       (Array.of_list list, transducer) in
  let add_to_transducer previous_word (current_word, current_output) =
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
     let rec loop i current_output transducer = begin
       if i = String.length current_word then
         ((), transducer)
       else
         let current_ch = String.get current_word i in
         let current_state = temp_states.(i) in
         let common_prefix = longest_common_prefix (output_str temp_states.(i) current_ch transducer |> value) current_output in
         let word_suffix = remainder common_prefix (output_str temp_states.(i) current_ch transducer |> value) in
         let transducer = set_output temp_states.(i) current_ch common_prefix transducer |> st in
         let transducer = List.fold_left (fun transducer (ch, next_state) ->
           set_output next_state ch (concat word_suffix (output_str next_state ch transducer |> value)) transducer |> st
         ) transducer (transitions current_state transducer |> value) in
         let transducer = if final temp_states.(i) transducer |> value then
           let strings = state_output temp_states.(i) transducer |> value in
           let updated_strings = String_set.map (fun s -> concat word_suffix s) strings in
           set_state_output temp_states.(i) updated_strings transducer |> st
         else transducer in
         let current_output = remainder common_prefix current_output in
         loop (i + 1) current_output transducer end in
       let* _  = loop 0 current_output in
       return current_word in
  let (current_word, transducer) = List.fold_left (fun (previous_word, transducer) (current_word, current_output) -> add_to_transducer previous_word (current_word, current_output) transducer) (previous_word, transducer) items in
  let rec loop i transducer =
     if i = 0 then
       transducer
     else
       let min_state, transducer = find_minimized temp_states.(i+1) transducer in
       let transducer = set_transition temp_states.(i) (String.get current_word i) min_state transducer |> st in
       loop (i-1) transducer in
   let transducer = loop (String.length current_word - 1) transducer in
   find_minimized temp_states.(0) transducer
