

module type S = sig
  type 'a t
  type state

  module Output_set: sig
   type t
  end

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val fold_left: ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t
  val cond: bool t -> if_true:(unit -> 'a t) -> if_false:(unit -> 'a t) -> 'a t

  val run: Char.t -> Char.t -> 'a t -> 'a
  val create_state: state t

  val final: state -> Bool.t t
  val set_final: state -> bool -> unit t

  val transition: state -> Char.t -> state Option.t t
  val transitions: state -> (Char.t * state) list t
  val set_transition: state -> Char.t -> state -> unit t

  val state_output: state -> Output_set.t t
  val set_state_output: state -> Output_set.t -> unit t

  val output: state -> Char.t -> String.t Option.t t
  val outputs: state -> (Char.t * string) list t
  val output_str: state -> Char.t -> String.t t
  val set_output: state -> Char.t -> String.t -> unit t

  val copy_state: state -> state t
  val clear_state: state -> unit t

  val member: state -> state option t
  val insert: state -> unit t

  val print_transducer: state -> String.t -> unit t

  val debug: unit t
  val accept: string -> state -> Output_set.t t
end

module Output_set = Set.Make(String)
let string_of_output_set s = Printf.sprintf "String_set [%s]" (Output_set.to_seq s |> List.of_seq |>String.concat "; ")
module Int_set = Set.Make(Int)

module Int_map = Map.Make(Int)

module Char_map = Map.Make(Char)


type transducer = {
  next_state: int;
  first_char: char;
  last_char: char;
  final_states: Int_set.t;
  transitions:  int Char_map.t Int_map.t;
  state_outputs: Output_set.t Int_map.t;
  outputs: string Char_map.t Int_map.t;
  dictionary: int list

}

type 'a t = transducer -> ('a * transducer)
type state = int

let run first_char last_char (f:'a t) =
  let transducer = {
    next_state = 0;
    first_char;
    last_char;
    final_states = Int_set.empty;
    transitions = Int_map.empty;
    state_outputs = Int_map.empty;
    outputs = Int_map.empty;
    dictionary = [];
  } in
  f transducer |> fst

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

let bind m f t = let (a, t) = (m t) in f a t

let map m f t = let (a, t) = (m t) in (f a, t)

let return a t = (a, t)

let value (a, _) = a
let st (_, state) = state

let (let*) = bind
let (>>|) = map
let (>>=) = bind

let cond pred ~if_true ~if_false =
  let* b = pred in
  if b then
    if_true ()
  else
    if_false ()

let fold_left f init l transducer =
  let (init, transducer) = init transducer in
  let rec loop l acc transducer =
    match l with
    | [] -> (acc, transducer)
    | x::rest ->
       let (res, transducer) = f acc x transducer in
       loop rest res transducer in
  loop l init transducer

let all l=
  fold_left (fun acc a t -> let (r, t) = a t in (r :: acc, t)) (return []) l >>| List.rev
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
    state_outputs = Int_map.add next_state Output_set.empty state_outputs;
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

let outputs state transducer =
  (Int_map.find state transducer.outputs |> Char_map.to_seq |> List.of_seq, transducer)

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

let copy_state state transducer =
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
    state_outputs;
    _
  } = transducer in
  ((), {
    transducer with
    final_states = Int_set.remove state final_states;
    transitions = Int_map.add state Char_map.empty transitions;
    (* TODO Should the outputs be removed, I can't see the value of keeping them
       without the transitions *)
    outputs = Int_map.add state Char_map.empty outputs;
    state_outputs = Int_map.add state Output_set.empty state_outputs;
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
        (* Compare the final state outputs for each transition *)
        let state_outputs_cmp = Output_set.compare (Int_map.find state1 transducer.state_outputs) (Int_map.find state2 transducer.state_outputs) in
        if state_outputs_cmp <> 0 then
         state_outputs_cmp
        else
        (* Compare the next state of each transition *)
        let states_1 = List.map (fun label -> Char_map.find label state1_transitions) labels in
        let states_2 = List.map (fun label -> Char_map.find label state2_transitions) labels in
        List.compare (fun s1 s2 ->compare_states s1 s2 transducer) states_1 states_2


let member state transducer =
  (List.find_opt (fun stored_state -> compare_states stored_state state transducer = 0 ) transducer.dictionary, transducer)

let insert state transducer =
   ((), { transducer with dictionary = state::transducer.dictionary })

let print_transducer state filename transducer =
  let visited = ref Int_set.empty in
  let oc = open_out filename in
  let state_id state = Printf.sprintf "state_%d" state in
  Printf.fprintf oc "digraph {";
  let rec loop state =
    if not (Int_set.mem state (!visited)) then begin
    visited := Int_set.add state (!visited);
    let transitions = Int_map.find state transducer.transitions  in
    let outputs = Int_map.find state transducer.outputs in
    let state_outputs = Int_map.find state transducer.state_outputs |> Output_set.to_seq |> List.of_seq |> String.concat "," in
    let labels = transitions |> Char_map.bindings |> List.map fst in
    let node_shape = if final state transducer |> value then "doublecircle" else "circle" in
    Printf.fprintf oc "%s [label = \"%d/%s\" shape = \"%s\"]\n" (state_id state) state state_outputs node_shape;
    List.iter (fun label ->
        let to_state = Char_map.find label transitions in
        let arc_output = Char_map.find label outputs in
        Printf.fprintf oc "%s -> %s [label = \"%c/%s\"]\n" (state_id state) (state_id to_state) label arc_output;
        loop to_state
        ) labels end in
  loop state;
  Printf.fprintf oc "}";
  close_out oc;
  ((), transducer)

let debug transducer =
  Printf.printf "Next_state: %d\n" transducer.next_state;
  Printf.printf "Final states:\n%s\n" (Int_set.to_seq transducer.final_states |> List.of_seq |> List.map Int.to_string |>String.concat ", ");
  Printf.printf "Transitions:\n";
  Int_map.iter (fun state transitions -> Printf.printf "%d: %s\n" state (Char_map.to_seq transitions |> List.of_seq |> List.map (fun (c, s) -> Printf.sprintf "%c->%d" c s) |> String.concat ", ")) transducer.transitions;
  Printf.printf "State Outputs:\n";
  Int_map.iter (fun state state_outputs -> Printf.printf "%d: %s\n" state (string_of_output_set state_outputs)) transducer.state_outputs;
  Printf.printf "Outputs:\n";
  Int_map.iter (fun state transitions -> Printf.printf "%d: %s\n" state (Char_map.to_seq transitions |> List.of_seq |> List.map (fun (c, s) -> Printf.sprintf "%c->%s" c s) |> String.concat ", ")) transducer.outputs;
  Printf.printf "Dictionary:\n %s\n\n" (List.map string_of_int transducer.dictionary |> String.concat ", ");
  ((), transducer)

let accept input start_state =
  let rec loop acc state i =
    if String.length input = i then
      let* is_final = final state in
      let* so = state_output state in
      if is_final then
         return (Output_set.map (fun o -> acc ^ o) so)
      else
         return Output_set.empty
    else
      let ch = String.get input i in
      let* next_state = transition state ch in
      match next_state with
      | None -> return Output_set.empty
      | Some next_state ->
        let* current_output = output_str state ch in
        let acc = String.concat "" [acc; current_output] in
        loop acc next_state (i+1) in
  loop "" start_state 0