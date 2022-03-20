
type ('compiled, 'output) transition = {
  ch: char;
  output: 'output;
  target: 'compiled
}

type ('compiled, 'output) t = {
   transitions: ('compiled, 'output) transition list;
   final_output: 'output Option.t;
}

let init = {
  transitions = [];
  final_output = None;
}

let empty_final_state ~empty_output= {
    transitions = [];
    final_output = Some empty_output;
}

let update_transition state transition =
  let ch = transition.ch in
  let filtered_transitions = List.filter (fun transition -> ch != transition.ch) state.transitions in
  let transitions = transition::filtered_transitions in
  { state with transitions }

let set_transition state ch output target =
  let new_transition = {
   ch; output=output; target;
  } in
  update_transition state new_transition

let get_transition state ch =
  let transitions = state.transitions in
  List.find_opt (fun transition -> transition.ch = ch) transitions

let get_output state ch =
  let transitions = state.transitions in
  List.find_map (fun transition -> if transition.ch = ch then Some transition.output else None) transitions

let get_outputs state =
  let transitions = state.transitions in
  List.map (fun transition -> (transition.ch, transition.output)) transitions

let set_output state ch output =
  let transition = get_transition state ch |> Option.get in
  let transition = { transition with output } in
  update_transition state transition

let update_transitions state ~f =
  let updated_transitions = List.map f state.transitions in
  { state with transitions = updated_transitions }


let is_final state =
  Option.is_some state.final_output

let set_final_output state final_output =
  { state with final_output = (Some final_output) }

let get_final_output state ~default =
  Option.value state.final_output ~default

let update_final_output state ~f =
  let final_output = Option.map f state.final_output in
  { state with final_output }

(*
let show state ~show_output ~show_compiled =
  let transitions = state.transitions in
  let s = List.map (fun {ch; output; target} -> Printf.sprintf "{ch = '%c', output = %s, target = %s}"
    ch (show_output output) (show_compiled target)) transitions |>  String.concat ", "  in
  Printf.sprintf "{final = %b, transitions = [ %s ], final_output = %s}"
    state.final) s (show_output state.final_output)
*)