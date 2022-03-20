open Lucene_fst

let test_remainder () : unit =
  let inputs = ["", "", ""; "ab","abc","c"] in
  List.iter (fun (s1, s2, expected) ->
    let result = String_output.subtract s1 s2 in
    Alcotest.(check string) "Remainder should be a suffix of the second input" result expected) inputs

let test_common (): unit =
  let inputs = [
    "", "", "";
    "ab", "abc", "ab";
    "abc", "ab", "ab";
    "ca", "co", "c";
    "ca", "c", "c"] in
  List.iter (fun (s1, s2, expected) ->
    let result = String_output.common s1 s2 in
    Alcotest.(check string) "Prefix length should be correct" result expected) inputs


let tests = [
  "Create remainder", `Quick, test_remainder;
  "Find longest common prefix", `Quick, test_common;
]