open Lucene_fst
open Lucene_data_input_2

(*
 Copied from
 https://stackoverflow.com/questions/53839695/how-do-i-read-the-entire-content-of-a-given-file-into-a-string  *)
let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s

let read_lines filename =
  let chin = open_in filename in
  let rec loop () =
    try
      let line = input_line chin in
        line :: loop ()
      with
      | End_of_file -> [] in
  loop ()

let spellings = read_lines "data/spellings.txt" |> List.map (fun s ->
 let l = String.split_on_char ' ' s in
 match l with
 | [w1; w2] -> (w1, w2)
 | _ -> failwith "Expect pairs"
)

module M = Reverse_bytes_source.Make(String_bytes)
module N = Data_input.Make(M)

let test_running_an_fst () =
  let module O = Int_output_reader.Make(N) in
  let module P = Byte_array_fst_reader.Make(N)(Int_output)(O) in
  let module Q = Byte_array_fst_reader_utils.Make(P) in

  let inputs = [
    "ca", 5;
    "car", 7;
    "cat", 12;
    "cataract", 3;
    "cataracts", 13;
    "co", 23;
    "cot", 2;
    "dog", 10;
    "dogs", 10;
    "dot", 10
  ] in
  let bytes = read_whole_file "data/fst-1.bytes" in
  let di = M.of_bytes bytes in
  let start_node = 45 in
  let empty_output = 0 in
  let fst_reader = P.create ~di ~start_node ~empty_output in
  List.iter (fun (input, expected) ->
    let path = Q.fst_match_term ~fst_reader input in
    let result = Q.make_output path in
    let test_message = Printf.sprintf "Expect result %d for input \"%s\"" expected input in
    Alcotest.(check int) test_message expected result
  ) inputs

let test_spelling_corrections () =
  let module O = String_output_reader.Make(N) in
  let module P = Byte_array_fst_reader.Make(N)(String_output)(O) in
  let module Q = Byte_array_fst_reader_utils.Make(P) in

  let bytes = read_whole_file "data/fst-2.bytes" in
  let di = M.of_bytes bytes in
  let start_node = 32136 in
  let empty_output = "" in
  let fst_reader = P.create ~di ~start_node ~empty_output in
  List.iter (fun (input, expected) ->
    let path = Q.fst_match_term ~fst_reader input in
    let result = Q.make_output path in
    let test_message = Printf.sprintf "Expect result %s for input \"%s\"" expected input in
    Alcotest.(check string) test_message expected result
  ) spellings

let test_read_all_arcs_in_a_linear_node () =
  let module O = Int_output_reader.Make(N) in
  let module P = Byte_array_fst_reader.Make(N)(Int_output)(O) in
  let module Q = Byte_array_fst_reader_utils.Make(P) in
  let bytes = read_whole_file "data/fst-4.bytes" in
  let di = M.of_bytes bytes in
  let start_node = 13 in
  let empty_output = 0 in
  let fst_reader = P.create ~di ~start_node ~empty_output in
  let expected_arcs = [
    { Arc.label=97; target=3; output=5; final_output=0 };
    { label=99; target=5; output=3; final_output=9 };
  ] in
  let arcs = P.read_arcs_at_target ~fst_reader start_node in
  Alcotest.(check int) "The node should contain two arcs\n" 2 (List.length arcs);
  Alcotest.(check int) "The arcs should be expected\n" 0 (Arc.compare_arc_lists ~compare_outputs:Int_output.compare arcs expected_arcs)

let test_read_all_arcs_in_a_direct_addressing_node () =
  let module O = Int_output_reader.Make(N) in
  let module P = Byte_array_fst_reader.Make(N)(Int_output)(O) in
  let module Q = Byte_array_fst_reader_utils.Make(P) in
  let bytes = read_whole_file "data/fst-3.bytes" in
  let di = M.of_bytes bytes in
  let start_node = 36 in
  let empty_output = 0 in
  let fst_reader = P.create ~di ~start_node ~empty_output in
  let expected_arcs = [
    { Arc.label=97; target=3; output=5; final_output=0 };
    { label=99; target=5; output=3; final_output=9 };
    { label=100; target=8; output=13; final_output=0 };
    { label=101; target=11; output=2; final_output=0 };
    { label=102; target=5; output=10; final_output=0 }
  ] in
  let arcs = P.read_arcs_at_target ~fst_reader start_node in
  Printf.printf "arcs: %s\n" (Arc.show_arcs ~show_output:Int_output.to_string arcs);
  Alcotest.(check int) "The node should contain two arcs\n" 5 (List.length arcs);
  Alcotest.(check int) "The arcs should be expected\n" 0 (Arc.compare_arc_lists ~compare_outputs:Int_output.compare arcs expected_arcs)

let tests = [
  "Run an fst", `Quick, test_running_an_fst;
  "Fun an fst for spelling corrections", `Slow, test_spelling_corrections;
  "Read all arcs in a linear node", `Quick, test_read_all_arcs_in_a_linear_node;
  "Read all arcs in a direct addressing node", `Quick, test_read_all_arcs_in_a_direct_addressing_node;
]
