(** Deserialised representation of the arcs in a byte array fst *)

type 'output t = {
  label: int;
  target: int;
  output: 'output;
  final_output: 'output;
}