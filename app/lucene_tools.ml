open Lucene_codec

let print_assoc_list_of_strings l =
  List.iter (fun (k, v) -> Printf.printf " %s -> %s \n" k v) l

let print_list_of_strings l =
  List.iter (fun s -> Printf.printf " %s\n" s) l

let print_index_header h =
  Printf.printf "Index Header\n";
  Printf.printf "Magic: %d; Name: %s; Version: %d \n" h.Codec_util.magic h.name h.version;
  Printf.printf "Object id: %s; Suffix: %s \n" h.object_id h.suffix_bytes


let read_segments_file () =
  let f = "/Users/sidharthkuruvila/src/lucene-playground/app/test-index/segments_10" in
  let segments = Segments.for_file f in
  let lucene_version = segments.lucene_version in
  let version = segments.version in
  let counter = segments.name_counter in
  let size = segments.seg_count in
  let ms_lucene_version = segments.ms_lucene_version in
  Printf.printf "major: %d; minor: %d; bugfix: %d\n"  lucene_version.major lucene_version.minor lucene_version.bugfix;
  Printf.printf "version: %s; counter: %s; size: %d\n" (Int64.to_string version) (Int64.to_string counter) size;
  Printf.printf "MS - major: %d; minor: %d; bugfix: %d\n"  ms_lucene_version.major ms_lucene_version.minor ms_lucene_version.bugfix

let read_segment_info_file () =
  let f = "/Users/sidharthkuruvila/src/lucene-playground/app/test-index/_z.si" in
  let segment_info = Segment_info.for_file f in
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
  ()

let _ =
  read_segments_file ();
  print_endline "";
  read_segment_info_file ()
