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

  let fst_match_term ~fst term =
    let fst_reader = fst in
    let start_arc = P.first_arc fst_reader in
    let target_length = String.length term in
    let rec loop prev_arc n =
      if n = target_length then
        [prev_arc]
      else
        let label = int_of_char (String.get term n) in
        let arc_option = P.read_next_arc label ~fst_reader ~arc:prev_arc in
        match arc_option with
        | Some arc -> prev_arc::(loop arc (n + 1))
        | None -> [prev_arc] in
    let path = loop start_arc 0 in
    path in

  let rec make_output path =
    match path with
    | [x] -> Int_output.add x.P.Arc.output x.P.Arc.final_output
    | x::rest ->
        Int_output.add x.P.Arc.output (make_output rest)
    | [] -> Int_output.empty in

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
  let fst = P.create ~di ~start_node ~empty_output in
  List.iter (fun (input, expected) ->
    let path = fst_match_term ~fst input in
    let result = make_output path in
    let test_message = Printf.sprintf "Expect result %d for input \"%s\"" expected input in
    Alcotest.(check int) test_message expected result
  ) inputs

let test_spelling_corrections () =
  let module O = String_output_reader.Make(N) in
  let module P = Byte_array_fst_reader.Make(N)(String_output)(O) in

  let fst_match_term ~fst term =
    let fst_reader = fst in
    let start_arc = P.first_arc fst_reader in
    let target_length = String.length term in
    let rec loop prev_arc n =
      if n = target_length then
        [prev_arc]
      else
        let label = int_of_char (String.get term n) in
        let arc_option = P.read_next_arc label ~fst_reader ~arc:prev_arc in
        match arc_option with
        | Some arc -> prev_arc::(loop arc (n + 1))
        | None -> [prev_arc] in
    let path = loop start_arc 0 in
    path in

  let rec make_output path =
    match path with
    | [x] -> String_output.add x.P.Arc.output x.P.Arc.final_output
    | x::rest ->
        String_output.add x.P.Arc.output (make_output rest)
    | [] -> String_output.empty in

  let bytes = read_whole_file "data/fst-2.bytes" in
  let di = M.of_bytes bytes in
  let start_node = 32136 in
  let empty_output = "" in
  let fst = P.create ~di ~start_node ~empty_output in
  List.iter (fun (input, expected) ->
    let path = fst_match_term ~fst input in
    let result = make_output path in
    let test_message = Printf.sprintf "Expect result %s for input \"%s\"" expected input in
    Alcotest.(check string) test_message expected result
  ) spellings

let tests = [
  "test running an fst", `Quick, test_running_an_fst;
  "test running an fst for corrections", `Slow, test_spelling_corrections;
]
