open Lucene_terms_enumerator

let hex_string_to_bytes s =
  String.split_on_char ' ' s
  |> List.map (fun s -> Scanf.sscanf s "%x" (fun x -> char_of_int x))
  |> List.to_seq |> String.of_seq

let cases = [
  hex_string_to_bytes "d3 bd 92 2 4 32 ce 4 72 d7 6 ce 8d a e0 8b 10",
  [
    'z', Some 1124998;
    'p', None;
  ];
  hex_string_to_bytes "db 82 b9 1 4 69 d1 4 6c 81 a 75 b1 d 7a 81 12",
  [
    'z', Some 760790;

  ];
  hex_string_to_bytes "b3 f7 b5 1 2 6d 99 5 72 ef a",
  [
    'z', Some 746223;
  ];
]

let test_block_pointer () =
  List.iter (fun (pointer_data, tests) ->
    List.iter (fun (ch, expected_position) ->
      let result_position = Block_pointer.find_block pointer_data ch in
      Alcotest.(check (option int)) "find_block should return the expected position" expected_position result_position;
    ) tests
  ) cases

let tests = [
  "Block pointers", `Quick, test_block_pointer;
]