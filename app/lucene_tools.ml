open Lucene_codec

let dir = "/Users/sidharthkuruvila/src/lucene-playground/app/catalog-index"

let print_assoc_list_of_strings l =
  List.iter (fun (k, v) -> Printf.printf " %s -> %s \n" k v) l

let print_list_of_strings l =
  List.iter (fun s -> Printf.printf " %s\n" s) l

let print_index_header h =
  Printf.printf "Index Header\n";
  Printf.printf "Magic: %d; Name: %s; Version: %d \n" h.Codec_util.magic h.name h.version;
  Printf.printf "Object id: %s; Suffix: %s \n" h.object_id h.suffix_bytes




let read_segments_file () =
  let f = Printf.sprintf "%s/%s" dir (Directory.get_segment_file dir) in
  let segments = Segments.for_file f in
  let lucene_version = segments.lucene_version in
  let version = segments.version in
  let counter = segments.name_counter in
  let size = segments.seg_count in
  let ms_lucene_version = segments.ms_lucene_version in
  Printf.printf "major: %d; minor: %d; bugfix: %d\n"  lucene_version.major lucene_version.minor lucene_version.bugfix;
  Printf.printf "version: %s; counter: %s; size: %d\n" (Int64.to_string version) (Int64.to_string counter) size;
  Printf.printf "MS - major: %d; minor: %d; bugfix: %d\n"  ms_lucene_version.major ms_lucene_version.minor ms_lucene_version.bugfix;
  segments

let read_segment_info_file prefix seg_id =
  let f = Printf.sprintf "/Users/sidharthkuruvila/src/lucene-playground/app/catalog-index/%s.si" prefix in
  let segment_info = Segment_info.for_file f ~name:prefix ~seg_id in
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
  print_assoc_list_of_strings attributes;
  segment_info


let read_field_infos_file prefix =
  let f = Printf.sprintf "/Users/sidharthkuruvila/src/lucene-playground/app/catalog-index/%s.fnm" prefix in
  let field_infos = Field_infos.for_file f in
  let size = Field_infos.field_infos_count field_infos in
  Printf.printf "Size: %d\n" size;
  field_infos

let _ =
  let segments = read_segments_file () in
  List.iter (fun segment ->
    let prefix = segment.Segments.Segment.seg_name in
    let seg_id = segment.seg_id in
    Printf.printf "Prefix: %s\n" prefix;
    print_endline "";
    let segment_info = read_segment_info_file prefix seg_id in
    print_endline "";
    let field_infos = read_field_infos_file prefix in
    let segment_read_state = Segment_read_state.make ~dir ~segment_info ~field_infos in
    ignore (Block_tree_terms_reader.create segment_read_state)
  ) segments.segments

