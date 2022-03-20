open Lucene_fst
let test_split_at_index () =
  let input = ["a"; "b"; "c"] in
  let index = 2 in
  let result_fst, result_snd = Lists.split_at_index input ~index in
  let expected_fst, expected_snd = (["a"; "b"], ["c"]) in
  Alcotest.(check (list string)) "The first part of the result should be the prefix" expected_fst result_fst;
  Alcotest.(check (list string)) "The second part of the result should be the suffix" expected_snd result_snd

let test_split_at_index_when_list_smaller_than_index () =
  let input = ["a"; "b"; "c"] in
  let index = 4 in
  let result_fst, result_snd = Lists.split_at_index input ~index in
  let expected_fst, expected_snd = (["a"; "b"; "c"], []) in
  Alcotest.(check (list string)) "The first part of the result should be the list" expected_fst result_fst;
  Alcotest.(check (list string)) "The second part of the result should be an empty list" expected_snd result_snd

let test_drop_n () =
  let input = ["a"; "b"; "c"] in
  let n = 2 in
  let result = Lists.drop_n input ~n in
  let expected = ["c"] in
  Alcotest.(check (list string)) "The result should be the suffix after dropping n items" expected result

let test_drop_n_when_list_smaller_than_n () =
  let input = ["a"; "b"; "c"] in
  let n = 4 in
  let result = Lists.drop_n input ~n in
  let expected = [] in
  Alcotest.(check (list string)) "The result should be an empty list" expected result

let test_common_prefix_length () =
  let tests = [
    "abc", "abcd", 3;
    "abcd", "abc", 3;
    "abcd", "abcd", 4;
    "abc", "xyz", 0;
  ] in
  List.iter ( fun (word1, word2, expected) ->
    let list1 = String.to_seq word1 |> List.of_seq in
    let list2 = String.to_seq word2 |> List.of_seq in
    let result = Lists.common_prefix_length list1 list2 ~compare:Char.compare in
    let message = Printf.sprintf "The common prefix length of \"%s\" and \"%s\" should be %d" word1 word2 expected in
    Alcotest.(check int) message expected result
  ) tests

let tests = [
  "Split a list at an index", `Quick, test_split_at_index;
  "Split a list at an index greater than its length", `Quick, test_split_at_index_when_list_smaller_than_index;
  "Drop the first n items in a list", `Quick, test_drop_n;
  "Drop the first n items in a list smaller than n", `Quick, test_drop_n_when_list_smaller_than_n;
  "Find the length of the common prefix of two lists", `Quick, test_common_prefix_length;
]