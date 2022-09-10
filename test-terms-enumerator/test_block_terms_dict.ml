open Lucene_data_input_2
open Lucene_terms_enumerator

module Block_terms_dict = Block_terms_dict.Make(Mmapped_file_data_input)

let with_fd filename ~f =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore (f fd);
  Unix.close fd

let input_file = "data/terms-dict.tim"

let test_seek_exact () =
  with_fd input_file ~f:(fun fd ->
    let bytes_source = Mmapped_file_bytes.from_fd fd in
    let data_input = Mmapped_file_data_input.of_bytes bytes_source in
    let (ls, _, _, _) = Block_terms_dict.seek_term ~data_input 736521 "c" in
    List.iter (fun s -> Printf.printf "%s\n" s) ls;
    Alcotest.(check bool) "" false true
  )


let test_read_block () =
  with_fd input_file ~f:(fun fd ->
    let bytes_source = Mmapped_file_bytes.from_fd fd in
    let data_input = Mmapped_file_data_input.of_bytes bytes_source in
    Mmapped_file_data_input.set_position data_input 5638;
    let rec loop _ =
      ignore (Block_terms_dict.read_block ~data_input); loop () in
    ignore (loop ());
    Alcotest.(check bool) "" false true
  )

let tests = [
(*  "Seek an exact match of a suffix in the dictionary", `Quick, test_seek_exact;*)
  "Read a block in the dictionary", `Quick, test_read_block;
]