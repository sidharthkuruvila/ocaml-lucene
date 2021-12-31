open Lucene_fst


let test_final () =
  let transducer = Fst.make 'a' 'z' in
  let (state, transducer) = Fst.create_state transducer in
  Alcotest.(check bool) "Final state should be false" (Fst.final transducer state) false;
  let transducer = Fst.set_final transducer state true in
  Alcotest.(check bool) "Final state should be true" (Fst.final transducer state) true;
  let transducer = Fst.set_final transducer state false in
  Alcotest.(check bool) "Final state should be true" (Fst.final transducer state) false

let test_transition () =
  let transducer = Fst.make 'a' 'z' in
  let (state1, transducer) = Fst.create_state transducer in
  let (state2, transducer) = Fst.create_state transducer in
  let transducer = Fst.set_transition transducer state1 'a' state2 in
  Alcotest.(check (option int)) "transition should return the transition state"  (Fst.transition transducer state1 'a') @@ Some state2

let test_state_output () =
  let transducer = Fst.make 'a' 'z' in
  let (state, transducer) = Fst.create_state transducer in
  let transducer = Fst.set_state_output transducer state (Fst.String_set.singleton "abc") in
  Alcotest.(check (list string)) "state_output should return the set of outputs"  (Fst.state_output transducer state |> Fst.String_set.to_seq |> List.of_seq) @@ ["abc"]

let test_gen_min_fst () =
(*  let items = ["ca", "bat"; "cat", "bat"; "car", "bat";  "co", "bat"; "dog", "bar"] |> List.sort (fun (a,_) (b, _) -> String.compare a b) in*)
  let items = ["car", "bat"; "cat", "bat"; "casamaran", "bat"; "car", "bat"; "cog", "bat"; "dog", "bar"] |> List.sort (fun (a,_) (b, _) -> String.compare a b) in
  let (start_state, transducer) = Fst.create_minimal_transducer 'a' 'z' items in
  Fst.print_transducer transducer start_state "out.dot"

let test_remainder () =
  let inputs = ["", "", ""; "ab","abc","c"] in
  List.iter (fun (s1, s2, expected) ->
    let result = Fst.remainder s1 s2 in
    Alcotest.(check string) "Remainder should be a suffix of the second input" result expected) inputs

let test_prefix_length () =
  let inputs = ["", "", 0; "ab", "abc", 2; "abc", "ab", 2; "ca", "co", 1] in
  List.iter (fun (s1, s2, expected) ->
    let result = Fst.common_prefix_length s1 s2 in
    Alcotest.(check int) "Prefix length should be correct" result expected) inputs
(*
let int_range s e =
  List.of_seq (Seq.unfold (fun n -> if n = e then None else Some (n, n + 1)) s)

let nums = List.concat_map (fun n -> [-1*n; n]) (int_range 1 1025)
let zigged_nums = (int_range 1 2049)

let test_zig_zag_encode_int () =
  List.iter2 (fun a b -> Alcotest.(check int) "Does the int zig?" (Bit_utils.zig_zag_encode_int a) b)
    nums zigged_nums


let test_zig_zag_decode_int () =
  List.iter2 (fun a b -> Alcotest.(check int) "Does the int unzig?" (Bit_utils.zig_zag_decode_int a) b)
    zigged_nums nums

let test_msb () =
  let tests = [
   0, 0;
   1, 1;
   2, 2;
   3, 2;
   0b101010, 6;
   0b10101000000011111, 17;
   0b1011000000110001100010001, 25;
  ] in
  List.iter (fun (n, expected) ->
  Alcotest.(check int ) (Printf.sprintf "msb %d = %d" n expected) expected (Bit_utils.msb n)) tests
*)
let tests = [
  "final and set_final should return and update the final flag for a state", `Quick, test_final;
  "Setting a transition should allow it to be returned", `Quick, test_transition;
  "Setting a state_outpu should allow it to be returned", `Quick, test_state_output;
  "Create a minimum fst", `Quick, test_gen_min_fst;
  "Create remainder", `Quick, test_remainder;
  "Find longest common prefix", `Quick, test_prefix_length;
  (*"zig_zag_decode_int should return un zig zagged ints", `Quick, test_zig_zag_decode_int;
  "msb should return the index of the most significant bit", `Quick, test_msb;*)
]