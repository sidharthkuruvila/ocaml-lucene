
module type S = sig
  type t

  module Output: Output.S

  val first_arc: t -> Output.t Arc.t
  val read_next_arc: int -> fst_reader:t -> arc:Output.t Arc.t -> (Output.t Arc.t * int Option.t) Option.t
end

module Make(M: S) = struct
  let fst_match_term ~fst_reader term =
    let start_arc = M.first_arc fst_reader in
    let target_length = String.length term in
    let rec loop prev_arc n =
      if n = target_length then
        [prev_arc]
      else
        let label = int_of_char (String.get term n) in
        let arc_option = M.read_next_arc label ~fst_reader ~arc:prev_arc in
        match arc_option with
        | Some arc -> prev_arc::(loop (fst arc) (n + 1))
        | None -> [prev_arc] in
    let path = loop start_arc 0 in
    path

  let rec make_output path =
    match path with
    | [x] -> M.Output.add (x.Arc.output) (x.Arc.final_output)
    | x::rest ->
        M.Output.add (x.Arc.output) (make_output rest)
    | [] -> M.Output.empty
end