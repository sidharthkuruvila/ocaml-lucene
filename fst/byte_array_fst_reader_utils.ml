
module type S = sig
  type t

  module Output: Output.S

  module Arc: sig
    type t
    val output: t -> Output.t
    val final_output: t -> Output.t
  end
  val first_arc: t -> Arc.t
  val read_next_arc: int -> fst_reader:t -> arc:Arc.t -> Arc.t Option.t
end

module Make(M: S) = struct
  let fst_match_term ~fst term =
    let fst_reader = fst in
    let start_arc = M.first_arc fst_reader in
    let target_length = String.length term in
    let rec loop prev_arc n =
      if n = target_length then
        [prev_arc]
      else
        let label = int_of_char (String.get term n) in
        let arc_option = M.read_next_arc label ~fst_reader ~arc:prev_arc in
        match arc_option with
        | Some arc -> prev_arc::(loop arc (n + 1))
        | None -> [prev_arc] in
    let path = loop start_arc 0 in
    path

  let rec make_output path =
    match path with
    | [x] -> M.Output.add (M.Arc.output x) (M.Arc.final_output x)
    | x::rest ->
        M.Output.add (M.Arc.output x) (make_output rest)
    | [] -> M.Output.empty
end