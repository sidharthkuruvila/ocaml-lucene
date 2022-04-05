(*open Lucene_codec*)

type t = {
  path: string
}



(* Find the latest segments file in a directory
   There can be multiple segment files if the directory is opened when lucene
   is updating *)
let get_segment_file dir =
  let files = Array.to_list(Sys.readdir dir) in
  List.find (fun s -> String.length s >= 8 && String.sub s 0 8 = "segments") files
(*
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


let open_segment segment =
  let prefix = segment.Segments.Segment.seg_name in
  let seg_id = segment.seg_id in
  ()
*)
(* Load the most recent segments file in a directory
   to extract the current index
*)
(*
let open_dir path =
  let segment_file_name = get_segment_file path in
  let segment_file_path = Printf.sprintf "%s/%s" path segment_file_name in
  let segments = Segments.for_file segment_file_path in
  List.map open_segment segments
*)

