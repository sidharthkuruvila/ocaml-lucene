open Lucene_utils

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

let tests = [
  "zig_zag_encode_int should return zig zagged ints", `Quick, test_zig_zag_encode_int;
  "zig_zag_decode_int should return un zig zagged ints", `Quick, test_zig_zag_decode_int;
  "msb should return the index of the most significant bit", `Quick, test_msb;
]