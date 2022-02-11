open Lucene_fst
let test_gen_min_fst () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let items = [
    "ca", "bat";
    "cat", "bat";
    "cat", "bar";
    "car", "bat";
    "co", "bat";
    "dog", "bar"] |> List.sort (fun (a,_) (b, _) -> String.compare a b) in
  ignore (
     Fst.run 'a' 'z' (
     let* start_state = Builder.create_minimal_transducer  items in
     let* _ = Fst.print_transducer start_state "out.dot" in
     let* results = Fst.fold_left (fun acc (i, o) -> let* res = Fst.accept i start_state in return ((i, res, o) :: acc)) (return []) items in
     List.iter (fun (i, res, o) ->
       let contained = Fst.Output_set.mem o res in
       Alcotest.(check bool) (Printf.sprintf "Expected %s for input %s got %s" o i (Fst.string_of_output_set res)) true contained) results;
     return ()))
(*  Fst.print_transducer transducer start_state "out.dot"*)

let read_lines filename =
  let chin = open_in filename in
  let rec loop () =
    try
      let line = input_line chin in
        line :: loop ()
      with
      | End_of_file -> [] in
  loop ()

let read_spellings filename : (string * (string list)) list =
  let lines = read_lines filename in
  let rec loop i l : (string * string list) list =
   match (i, l) with
   | (word::rest, _) when String.get word 0 = '$' ->
   let correct_spelling = String.sub word 1 (String.length word - 1) in
         loop rest ((correct_spelling, [])::l)
   | (word::rest, []) ->
      let correct_spelling = String.sub word 1 (String.length word - 1) in
      loop rest ((correct_spelling, [])::l)
   | (word::rest, (correct_spelling, misspellings)::rest_l) ->
      loop rest ((correct_spelling, word::misspellings)::rest_l)
   | ([], _) -> l in
  loop lines []

let test_spellings () =
  let module Fst = Fst.Make(String_output) in
  let open Fst in
  let module Builder = Acyclic_transducer.Make(Fst) in
  let spellings = read_spellings "data/spelling-corrections.txt" in
  let mappings: (string * string) list = List.concat_map (fun (c, ms) -> List.map (fun m -> (m, c)) ms) spellings
    |> List.sort (fun (a, _) (b, _) -> String.compare a b) in
  List.iter (fun (i, o) -> Printf.printf "%s -> %s\n" i o) mappings;
    ignore (
  Fst.run 'a' 'z' (
       let* start_state = Builder.create_minimal_transducer  mappings in
       let* _ = Fst.print_transducer start_state "out.dot" in
       let* results = Fst.fold_left (fun acc (i, o) -> let* res = Fst.accept i start_state in return ((i, res, o) :: acc)) (return []) mappings in
       List.iter (fun (i, res, o) ->
         let contained = Fst.Output_set.mem o res in
                Alcotest.(check bool) (Printf.sprintf "Expected %s for input %s got %s" o i (Fst.string_of_output_set res)) true contained) (List.rev results);
       return ()))

let tests = [
  "Create a minimum fst", `Quick, test_gen_min_fst;
  "Test against a spellings dictionary", `Quick, test_spellings;
]