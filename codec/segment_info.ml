open Lucene_data_input

type t = {
  name: string;
  seg_id: string;
  index_header: Codec_util.index_header;
  version: Codec_util.lucene_version;
  min_version: Codec_util.lucene_version option;
  doc_count: int;
  is_compound_file: bool;
  diagnostic_map: (string * string) list;
  files: string list;
  attributes: (string * string) list;
  sort_fields: string list;
}


let print_assoc_list_of_strings l =
  List.iter (fun (k, v) -> Printf.printf " %s -> %s \n" k v) l

let print_list_of_strings l =
  List.iter (fun s -> Printf.printf " %s\n" s) l


let print_index_header h =
  Printf.printf "Index Header\n";
  Printf.printf "Magic: %d; Name: %s; Version: %d \n" h.Codec_util.magic h.name h.version;
  Printf.printf "Object id: %s; Suffix: %s \n" h.object_id h.suffix_bytes

let display segment_info =
  let index_header = segment_info.index_header in
  let lucene_version = segment_info.version in
  let doc_count = segment_info.doc_count in
  let is_compound_file = if segment_info.is_compound_file then "yes" else "no" in
  let diagnostic_map = segment_info.diagnostic_map in
  let files = segment_info.files in
  let attributes = segment_info.attributes in
  print_index_header index_header;
  Printf.printf "major: %d; minor: %d; bugfix: %d\n"  lucene_version.major lucene_version.minor lucene_version.bugfix;
  Printf.printf "doc count: %d; is compound file: %s\n"  doc_count is_compound_file;
  print_endline "Diagnostic information";
  print_assoc_list_of_strings diagnostic_map;
  print_endline "Files";
  print_list_of_strings files;
  print_endline "Attributes";
  print_assoc_list_of_strings attributes

let read_lucene_version di =
  let major = Index_input.read_int di in
  let minor = Index_input.read_int di in
  let bugfix = Index_input.read_int di in
  { Codec_util.major; minor; bugfix }


let for_data_input ~name ~seg_id di =
  let index_header = Codec_util.read_header di in
  let version = read_lucene_version di in
  let has_min_version = Index_input.read_byte di = 1 in
  let min_version = if has_min_version then
    Some (read_lucene_version di)
  else
    None in
  let doc_count = Index_input.read_int di in
  let is_compound_file = Index_input.read_byte di = 1 in
  let diagnostic_map = Index_input.read_assoc_list_of_strings di in
  let files = Index_input.read_list_of_strings di in
  let attributes = Index_input.read_assoc_list_of_strings di in
  let sort_fields = Index_input.read_list_of_strings di in
  Codec_util.check_footer di;
  {
    index_header;
    version;
    min_version;
    doc_count;
    is_compound_file;
    diagnostic_map;
    files;
    attributes;
    sort_fields;
    name;
    seg_id;
  }

let read dir prefix seg_id =
  let filename = Printf.sprintf "%s.si" prefix in
  Directory.open_input_with ~f:(for_data_input ~name:prefix ~seg_id) dir filename

