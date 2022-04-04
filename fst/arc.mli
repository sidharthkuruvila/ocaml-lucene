(** Deserialised representation of the arcs in a byte array fst *)

type 'output t = {
  label: int;
  target: int;
  output: 'output;
  final_output: 'output;
}

val compare_arc_lists: compare_outputs:('output -> 'output -> int) -> 'output t list -> 'output t list -> int

val show_arcs: show_output:('output -> String.t) -> 'output t list -> String.t