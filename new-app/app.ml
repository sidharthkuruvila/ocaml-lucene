open Lucene_segment
open Lucene_terms_enumerator
open Lucene_data_input_2
open Lucene_fst

let directory = "/Users/sidharth/code/archive/redbubble/lucene-playground/app/catalog-index"

module Sliced_bytes = Sliced_bytes.Make(Mmapped_file_bytes)
module Mmapped_file_bytes_source = Bytes_source.Make(Mmapped_file_bytes)
module Mmapped_file_data_input = Data_input.Make(Mmapped_file_bytes_source)
module Block_terms_dict = Block_terms_dict.Make(Mmapped_file_data_input)
module Fst_bytes_source = Reverse_bytes_source.Make(Sliced_bytes)
module Fst_data_input = Data_input.Make(Fst_bytes_source)

module M = Field_infos_reader.Make(Mmapped_file_data_input)
module N = Meta_file_reader.Make(Mmapped_file_data_input)
module O = Segment_file_reader.Make(Mmapped_file_data_input)

module Fst_reader = Byte_array_fst_reader.Make(Fst_data_input)(String_output)(String_output_reader.Make(Fst_data_input))
module Fst_utils = Byte_array_fst_reader_utils.Make(Fst_reader)

module DirectoryF(Data_input: Data_input.S) = struct

  type t = String.t

  let segment_suffix = "_Lucene84_0"

  let with_fd filename ~f =
    let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
    let r = (f fd) in
    Unix.close fd;
    r

  let with_data_input filename ~f =
    with_fd filename ~f:(fun fd ->
       let bytes_source = Mmapped_file_bytes.from_fd fd in
       let data_input = Mmapped_file_bytes_source.of_bytes bytes_source in
       f data_input
    )

  let with_segment_file ~directory ~segment_name ~f =
    let segment_file_path = Printf.sprintf "%s/%s" directory (Printf.sprintf "%s.si" segment_name) in
    with_data_input segment_file_path ~f

  let with_field_infos_file ~directory ~segment_name ~f =
    let field_infos_path = Printf.sprintf "%s/%s" directory (Printf.sprintf "%s.fnm" segment_name) in
    with_data_input field_infos_path ~f

  let with_meta_file ~directory ~segment_name ~f =
    let meta_file_path = Printf.sprintf "%s/%s" directory (Printf.sprintf "%s%s.tmd" segment_name segment_suffix) in
    with_data_input meta_file_path ~f

  let with_index_file ~directory ~segment_name ~f =
    let index_file_path = Printf.sprintf "%s/%s" directory (Printf.sprintf "%s%s.tip" segment_name segment_suffix) in
    with_data_input index_file_path ~f

  let with_dict_file ~directory ~segment_name ~f =
    let index_file_path = Printf.sprintf "%s/%s" directory (Printf.sprintf "%s%s.tim" segment_name segment_suffix) in
    with_data_input index_file_path ~f
end


module Directory = DirectoryF(Mmapped_file_data_input)

let () =
  let segment_name = "_m" in
  let segment_id = [ 61; 20; 221; 26; 252; 52; 191; 141; 200; 188; 60; 92; 151; 43; 50; 57 ]
    |> List.map char_of_int |> List.to_seq |> String.of_seq in
  let version = 6 in
  let max_doc_count = 70000 in
  let segment_info = Directory.with_segment_file ~directory ~segment_name
    ~f:(fun data_input -> O.read ~data_input) in
  let field_infos = Directory.with_field_infos_file ~directory ~segment_name
    ~f:(fun data_input -> M.read ~data_input) in
  let meta_info = Directory.with_meta_file ~directory ~segment_name
    ~f:(fun data_input -> N.read ~data_input ~segment_id ~field_infos ~version ~max_doc_count) in
  Printf.printf "segment info: %s\n" (Segment_file_reader.show segment_info);
  Printf.printf "field_infos: %s\n" (Field_infos_reader.show field_infos);
  Printf.printf "meta_info: %s\n" (Meta_file_reader.show meta_info);
  let field_info = Field_infos_reader.find_field_by_name field_infos "title" in
  let field_number = field_info.Field_infos_reader.Field_info.field_number in
  let field_meta = Meta_file_reader.find_field_by_id meta_info field_number in
  Printf.printf "field_infos: %s\n" (Field_infos_reader.Field_info.show field_info);
  Printf.printf "meta_info: %s\n" (Meta_file_reader.Field_meta.show field_meta);
  Directory.with_index_file ~directory ~segment_name ~f:(fun data_input ->
  Directory.with_dict_file ~directory ~segment_name ~f:(fun dict_data_input ->
    let index_start_fp = field_meta.index_start_fp in
    let fst_meta = field_meta.fst_meta in
    let bytes = Mmapped_file_bytes_source.to_bytes data_input in
    let fst_bytes = Sliced_bytes.slice bytes index_start_fp fst_meta.num_bytes in
    let fst_data_input = Fst_bytes_source.of_bytes fst_bytes in
    let fst_reader = Fst_reader.create ~di:fst_data_input ~start_node:fst_meta.start_node ~empty_output:String_output.empty in
    let arcs = Fst_reader.read_arcs_at_target ~fst_reader fst_meta.start_node in
    Printf.printf "Arcs: %s\n" (Arc.show_arcs ~show_output:String_output.to_string arcs);
(*    Fst_utils.dottify ~fst_reader "out.dot";*)
    let output = Fst_utils.fst_match_term ~fst_reader "ab" |> Fst_utils.make_output in
    let block = Block_pointer.find_block output 'c' in
    Printf.printf "block address: %d; data dict length: %d \n" (Option.value ~default:(-1) block) (Mmapped_file_data_input.length dict_data_input);
    let (suffixes, _, _, _) = Block_terms_dict.seek_term ~data_input:dict_data_input (Option.value ~default:(-1) block) "c" in
    Printf.printf "sufixes: %s \n" (String.concat "," suffixes)
    (* 736521 *)
    (* 736521 *)
  ))
