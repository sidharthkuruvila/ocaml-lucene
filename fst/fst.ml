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

let show_dictionary { dictionary; _} = (String.concat ", " (List.map (fun n -> Printf.sprintf "%d" n) dictionary))

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

let final { final_states; _} state =
  Int_set.mem state final_states

let set_final transducer state flag =
  let final_states = if flag then
    Int_set.add state transducer.final_states
  else
    Int_set.remove state transducer.final_states in
  { transducer with final_states }

let transition { transitions; _ } state char =
  let state_transitions = Int_map.find state transitions in
  Char_map.find_opt char state_transitions

let transitions { transitions; _ } state =
  Int_map.find state transitions |> Char_map.to_seq |> List.of_seq

let set_transition transducer state char new_state =
  let transitions = transducer.transitions in
  let state_transitions = Int_map.find state transitions in
  let updated_state_transitions = Char_map.add char new_state state_transitions in
  let transitions = Int_map.add state updated_state_transitions transitions in
  { transducer with transitions }

let state_output { state_outputs; _ } state =
  Int_map.find state state_outputs

let set_state_output transducer state outputs =
  let state_outputs = Int_map.add state outputs transducer.state_outputs in
  { transducer with state_outputs }

let output { outputs; _ } state char =
  let state_outputs = Int_map.find_opt state outputs in
    Option.bind state_outputs (fun state_outputs -> Char_map.find_opt char state_outputs)

let output_str transducer state char =
  match output transducer state char with
  | None -> ""
  | Some s -> s

let set_output transducer state char output =
  let outputs = transducer.outputs in
  let outputs = match Int_map.find_opt state outputs with
  | None -> Int_map.add state (Char_map.singleton char output) outputs
  | Some state_transitions ->
    let updated_state_transitions = Char_map.add char output state_transitions in
    Int_map.add state updated_state_transitions outputs in
  { transducer with outputs }

let copy_state transducer state =
  let {
    next_state;
    final_states;
    transitions;
    state_outputs;
    outputs;
    _
  } = transducer in
  let final_states = if final transducer state then Int_set.add next_state final_states else final_states in
  let transitions = Int_map.add next_state (Int_map.find state transitions) transitions in
  let state_outputs = Int_map.add next_state (Int_map.find state state_outputs) state_outputs in
  let outputs = Int_map.add next_state (Int_map.find state outputs) outputs in
  (next_state, { transducer with next_state = next_state + 1; transitions; state_outputs; outputs; final_states })

let clear_state transducer state =
  let {
    final_states;
    transitions;
    outputs;
    _
  } = transducer in
  {
    transducer with
    final_states = Int_set.remove state final_states;
    transitions = Int_map.add state Char_map.empty transitions;
    (* TODO Should the outputs be removed, I can't see the value of keeping them
       without the transitions *)
    outputs = Int_map.add state Char_map.empty outputs;
  }

(* TODO Should we pass separate copies of the transducer
   for each state

   TODO This implementation is going to be very slow. It would be be better to move to a hashtable
   based impementation as lucene has.
   *)
let rec compare_states transducer state1 state2 =
  (* Use comparision to check if both states are final states or not *)
  let compare_final_states state1 state2 =
    let state1_final = final transducer state1 |> Bool.to_int in
    let state2_final = final transducer state2 |> Bool.to_int in
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
        List.compare (compare_states transducer) states_1 states_2


let member transducer state =
  List.find_opt (fun stored_state -> compare_states transducer stored_state state = 0) transducer.dictionary

let insert transducer state =
   { transducer with dictionary = state::transducer.dictionary }



let print_transducer transducer state filename =
  let oc = open_out filename in
  let state_id state = Printf.sprintf "state_%d" state in
  Printf.fprintf oc "digraph {";
  let rec loop state =
    let transitions = Int_map.find state transducer.transitions  in
    let outputs = Int_map.find state transducer.outputs in
    let labels = transitions |> Char_map.bindings |> List.map fst in
    let node_shape = if final transducer state then "doublecircle" else "circle" in
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



 let find_minimized transducer state =
   let r = member transducer state in
   match r with
   | None ->
    let (copy, transducer) = copy_state transducer state in
    let transducer = insert transducer copy in
    (copy, transducer)
   | Some state ->
      (state, transducer)


let common_prefix_length s1 s2 =
 let n = Int.min (String.length s1) (String.length s2) in
 let rec loop i =
   if i = n || not (Char.equal (String.get s1 i) (String.get s2 i)) then i
   else loop (i + 1) in
 loop 0


let rec make_n_states transducer n =
  if n = 0 then ([], transducer)
  else
    let (state, transducer) = create_state transducer in
    let (list, transducer) = make_n_states transducer (n - 1) in
    (state :: list, transducer)

let longest_common_prefix s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s1 0 l

(* Return what is left from the second string
once the common prefix is removed *)
let remainder s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s2 l (String.length s2 - l)

let concat s1 s2 = s1 ^ s2

let minimize_suffix transducer temp_states word prefix_length =
  let rec loop transducer i =
    if i = prefix_length - 1 then
      transducer
    else
      let ch = String.get word i in
      let next_state = temp_states.(i+1) in
      let prev_state = temp_states.(i) in
      let min_state, transducer = find_minimized transducer next_state in
      let transducer = set_transition transducer prev_state ch min_state in
      loop transducer (i - 1) in
 loop transducer (String.length word - 1)

let initialize_tail_states transducer temp_states word prefix_length =
  let rec loop transducer i =
    if i = String.length word then
      transducer
    else
      let ch = String.get word i in
      let prev_state = temp_states.(i) in
      let next_state = temp_states.(i+1) in
      let transducer = clear_state transducer next_state in
      let transducer = set_transition transducer prev_state ch next_state in
      loop transducer (i + 1) in
  loop transducer prefix_length

let create_minimal_transducer first_char last_char items =
  let max_width = List.fold_left (fun current_max (word, _) -> Int.max current_max (String.length word)) 0 items in
  let transducer = make first_char last_char in
  let previous_word = "" in
  let (temp_states, transducer) =
       let (list, transducer) = make_n_states transducer (max_width + 1) in
       (Array.of_list list, transducer) in
  let add_to_transducer (previous_word, transducer) (current_word, current_output) =
    let prefix_length = common_prefix_length previous_word current_word in
    (* Minimise the suffix of the previous word *)
     let transducer = minimize_suffix transducer temp_states previous_word prefix_length in
     (* Initialize tail states for current word *)
     let transducer = initialize_tail_states transducer temp_states current_word prefix_length in
     (* Set last char transition of current word as final *)
     let transducer = if not (String.equal current_word previous_word) then
       let transducer = set_final transducer temp_states.(String.length current_word) true in
       let transducer = set_state_output transducer temp_states.(String.length current_word) (String_set.singleton "") in
       transducer
     else transducer in
     let rec loop transducer i current_output = begin
       if i = String.length current_word then
         transducer
       else
         let current_ch = String.get current_word i in
         let current_state = temp_states.(i) in
         let common_prefix = longest_common_prefix (output_str transducer temp_states.(i) current_ch) current_output in
         let word_suffix = remainder common_prefix (output_str transducer temp_states.(i) current_ch) in
         let transducer = set_output transducer temp_states.(i) current_ch common_prefix in
         let transducer = List.fold_left (fun transducer (ch, next_state) ->
           set_output transducer next_state ch (concat word_suffix (output_str transducer next_state ch))
         ) transducer (transitions transducer current_state) in
         let transducer = if final transducer temp_states.(i) then
           let strings = state_output transducer temp_states.(i) in
           let updated_strings = String_set.map (fun s -> concat word_suffix s) strings in
           set_state_output transducer temp_states.(i) updated_strings
         else transducer in
         let current_output = remainder common_prefix current_output in
         loop transducer (i + 1) current_output end in
       let transducer = loop transducer 0 current_output in
       (current_word, transducer) in
  let (current_word, transducer) = List.fold_left add_to_transducer (previous_word, transducer) items in
  let rec loop transducer i =
     if i = 0 then
       transducer
     else
       let min_state, transducer = find_minimized transducer temp_states.(i+1) in
       let transducer = set_transition transducer temp_states.(i) (String.get current_word i) min_state in
       loop transducer (i-1) in
   let transducer = loop transducer (String.length current_word - 1) in
   find_minimized transducer temp_states.(0)
