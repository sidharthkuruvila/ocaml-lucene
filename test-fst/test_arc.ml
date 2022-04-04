open Lucene_fst

let compare_arc_lists () =
  let compare_outputs = Int.compare in
  let arc = {
    Arc.label = 50;
    target = 50;
    output = 50;
    final_output = 50;
  } in
  let cases = [
   [], [], 0;
   [arc], [], 1;
   [], [arc], -1;
   [arc], [arc], 0;
   [arc], [{arc with label=40}], 1;
   [arc], [{arc with label=60}], -1;
   [arc], [{arc with target=40}], 1;
   [arc], [{arc with target=60}], -1;
   [arc], [{arc with output=40}], 1;
   [arc], [{arc with output=60}], -1;
   [arc], [{arc with final_output=40}], 1;
   [arc], [{arc with final_output=60}], -1;
  ] in
  List.iter (fun (l1, l2, expected) ->
    let result = Arc.compare_arc_lists ~compare_outputs l1 l2 in
    Alcotest.(check int) "Results did not match when comparing lists" expected result) cases


let tests = [
  "Compare two lists of arcs", `Quick, compare_arc_lists;
]