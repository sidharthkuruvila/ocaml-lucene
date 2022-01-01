open Lucene_fst

let test_gen_min_fst () =
  let module Builder = Acyclic_transducer.Make(Fst) in
(*  let items = ["ca", "bat"; "cat", "bat"; "car", "bat";  "co", "bat"; "dog", "bar"] |> List.sort (fun (a,_) (b, _) -> String.compare a b) in*)
  let items = ["car", "bat"; "cat", "bat"; "catamaran", "bat"; "car", "bat"; "cog", "bat"; "dog", "bar"] |> List.sort (fun (a,_) (b, _) -> String.compare a b) in
  ignore (Builder.create_minimal_transducer 'a' 'z' items)
(*  Fst.print_transducer transducer start_state "out.dot"*)


let test_remainder () : unit =
  let inputs = ["", "", ""; "ab","abc","c"] in
  List.iter (fun (s1, s2, expected) ->
    let result = Acyclic_transducer.remainder s1 s2 in
    Alcotest.(check string) "Remainder should be a suffix of the second input" result expected) inputs

let test_prefix_length (): unit =
  let inputs = ["", "", 0; "ab", "abc", 2; "abc", "ab", 2; "ca", "co", 1] in
  List.iter (fun (s1, s2, expected) ->
    let result = Acyclic_transducer.common_prefix_length s1 s2 in
    Alcotest.(check int) "Prefix length should be correct" result expected) inputs

let tests = [
  "Create a minimum fst", `Quick, test_gen_min_fst;
  "Create remainder", `Quick, test_remainder;
  "Find longest common prefix", `Quick, test_prefix_length;
]