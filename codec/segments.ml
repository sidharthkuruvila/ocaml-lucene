open Lucene_data_input

type lucene_version = (int * int * int)

module Segment = struct
  type t = {
    seg_name: string;
    seg_id: string;
    seg_codec: string;
    del_gen: Int64.t;
    del_count: int;
    field_infos_gen: Int64.t;
    doc_values_gen: Int64.t;
    soft_del_count: int;
    sci_id: string option;
    field_infos_files: string list;
    doc_values_updates_files: (int * string list) list;
  }

end

type t = {
  lucene_version: Codec_util.lucene_version;
  index_created_major_version: int;
  version: Int64.t;
  name_counter: Int64.t;
  seg_count: int;
  ms_lucene_version: Codec_util.lucene_version;
  segments: Segment.t list;
  user_data: (string * string) list;
}

let display segments =
  let lucene_version = segments.lucene_version in
  let version = segments.version in
  let counter = segments.name_counter in
  let size = segments.seg_count in
  let ms_lucene_version = segments.ms_lucene_version in
  Printf.printf "major: %d; minor: %d; bugfix: %d\n"  lucene_version.major lucene_version.minor lucene_version.bugfix;
  Printf.printf "version: %s; counter: %s; size: %d\n" (Int64.to_string version) (Int64.to_string counter) size;
  Printf.printf "MS - major: %d; minor: %d; bugfix: %d\n"  ms_lucene_version.major ms_lucene_version.minor ms_lucene_version.bugfix

let read_doc_values_updates_files di =
  let count = Index_input.read_int di in
  let rec loop n =
    if n = 0 then
      []
    else
      let key = Index_input.read_int di in
      let value = Index_input.read_list_of_strings di in
      (key, value) :: loop (n - 1) in
  loop count

let for_data_input di =
  let _ = Codec_util.read_header di in
  let lucene_version = Codec_util.read_lucene_version di in
  let index_created_major_version = Index_input.read_vint di in
  let version = Index_input.read_long di in
  let name_counter = Index_input.read_vlong di in
  let seg_count = Index_input.read_int di in
  let ms_lucene_version = Codec_util.read_lucene_version di in
  let read_segments _ =
    let seg_name = Index_input.read_string di in
    let seg_id = Index_input.read_bytes di Codec_util.id_length in
    let seg_codec = Index_input.read_string di in
    let del_gen = Index_input.read_long di in
    let del_count = Index_input.read_int di in
    let field_infos_gen = Index_input.read_long di in
    let doc_values_gen = Index_input.read_long di in
    let soft_del_count = Index_input.read_int di in
    let has_scid = Index_input.read_byte di = 1 in
    let sci_id = if has_scid then
      Some (Index_input.read_bytes di Codec_util.id_length)
    else
      None in
    let field_infos_files = Index_input.read_list_of_strings di in
    let doc_values_updates_files = read_doc_values_updates_files di in
    [{
      Segment.seg_name;
      seg_id;
      seg_codec;
      del_gen;
      del_count;
      field_infos_gen;
      doc_values_gen;
      soft_del_count;
      sci_id;
      field_infos_files;
      doc_values_updates_files;
    }] in
  let segments = read_segments seg_count in
  let user_data = Index_input.read_assoc_list_of_strings di in
  Codec_util.check_footer di;
  {
    lucene_version;
    index_created_major_version;
    version;
    name_counter;
    seg_count;
    ms_lucene_version;
    segments;
    user_data;
  }


let get_segment_file dir =
  let files = Array.to_list(Sys.readdir dir) in
  List.find (fun s -> String.length s >= 8 && String.sub s 0 8 = "segments") files

(** Find the most recent "segments" file in the
  directory. There can be more than one if lucene is
  updating the index at the moment *)

let latest dir =
  Directory.open_input_with ~f:for_data_input dir (get_segment_file dir)