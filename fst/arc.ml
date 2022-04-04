type 'output t = {
  label: int;
  target: int;
  output: 'output;
  final_output: 'output;
}

let compare_arcs ~compare_outputs arc1 arc2 =
  let label_compare = Int.compare arc1.label arc2.label in
  if label_compare <> 0 then
    label_compare
  else
    let target_compare = Int.compare arc1.target arc2.target in
    if target_compare <> 0 then
      target_compare
    else
      let output_compare = compare_outputs arc1.output arc2.output in
      if output_compare <> 0 then
        output_compare
      else
        compare_outputs arc1.final_output arc2.final_output

let rec compare_arc_lists ~compare_outputs list1 list2 =
  let compare = compare_arcs ~compare_outputs in
  match (list1, list2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | arc1::rest1, arc2::rest2 ->
    let cmp = compare arc1 arc2 in
    if cmp <> 0 then cmp
    else compare_arc_lists ~compare_outputs rest1 rest2


let show_arc ~show_output {label; target; output; final_output } =
  Printf.sprintf "{ label=%d; target=%d; output=%s; final_output=%s }"
    label target (show_output output) (show_output final_output)

let show_arcs ~show_output arcs =
  Printf.sprintf "[ %s ]" (String.concat ";" (List.map (show_arc ~show_output) arcs))
