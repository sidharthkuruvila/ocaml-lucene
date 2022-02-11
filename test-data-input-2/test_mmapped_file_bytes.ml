open Lucene_data_input_2


let input_file = "data/test-01.txt"

let with_fd filename ~f =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore (f fd);
  Unix.close fd

let test_getting_a_single_byte () =
  with_fd input_file ~f:(fun fd ->
    let src = Mmapped_file_bytes.from_fd fd in
    let result = Mmapped_file_bytes.get_byte src 10 in
    let expected = 's' in
    Alcotest.(check char) "Char located at index 10 should be 's'" expected result
  )

let test_copying_bytes () =
  with_fd input_file ~f:(fun fd ->
    let src = Mmapped_file_bytes.from_fd fd in
    let dest = Bytes.of_string "Anythings" in
    Mmapped_file_bytes.copy_bytes src dest ~src_index:2 ~dest_index:2 ~length:5;
    let expected = "Anis isgs" in
    let result = Bytes.to_string dest in
    Alcotest.(check string) "Updated string should be \"Anis isgs\"" expected result
  )

let test_length () =
  with_fd input_file ~f:(fun fd ->
    let src = Mmapped_file_bytes.from_fd fd in
    let result = Mmapped_file_bytes.length src in
    let expected = 33 in
    Alcotest.(check int) "Char located at index 10 should be 's'" expected result
  )
let tests = [
  "Getting a single byte", `Quick, test_getting_a_single_byte;
  "Copying into a byte array", `Quick, test_copying_bytes;
  "The length of a mem mapped file", `Quick, test_length;
]