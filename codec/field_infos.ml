open Lucene_data_input

let mask_store_term_vector = 0x1
let mask_omit_norms = 0x2
let mask_store_payloads = 0x4
let mask_soft_deletes_field = 0x8

let format_selective_indexing = 2

module Index_options = struct
  type t =
   | NONE
   | DOCS
   | DOCS_AND_FREQS
   | DOCS_AND_FREQS_AND_POSITIONS
   | DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS

  let from_code code =
   match code with
   | 0 -> NONE
   | 1 -> DOCS
   | 2 -> DOCS_AND_FREQS
   | 3 -> DOCS_AND_FREQS_AND_POSITIONS
   | 4 -> DOCS_AND_FREQS_AND_POSITIONS_AND_OFFSETS
   | _ -> failwith "Code does not match"
end

module Doc_values_types = struct
  type t =
    | NONE
    | NUMERIC
    | BINARY
    | SORTED
    | SORTED_NUMERIC
    | SORTED_SET

  let from_code code =
    match code with
    | 0 -> NONE
    | 1 -> NUMERIC
    | 2 -> BINARY
    | 3 -> SORTED
    | 4 -> SORTED_NUMERIC
    | 5 -> SORTED_SET
    | _ -> failwith "Code does not match"
end

type field_info = {
  name: string;
  field_number: int;
  store_term_vector: bool;
  omit_norms: bool;
  store_payloads: bool;
  is_soft_deletes_field: bool;
  index_options: Index_options.t;
  doc_values_type: Doc_values_types.t;
  doc_values_gen: Int64.t;
  attributes: (string * string) list;
  point_data_dimension_count: int;
  point_index_dimension_count: int;
  point_num_bytes: int;
}

type t = {
  field_infos: field_info list
}

let field_infos_count field_infos =
  List.length field_infos.field_infos

let read_point_data version di =
  let point_data_dimension_count = Index_input.read_vint di in
  if point_data_dimension_count <> 0 then
    let point_index_dimension_count =
      if version >= format_selective_indexing then
        Index_input.read_vint di
      else
        point_data_dimension_count in
    let point_num_bytes = Index_input.read_vint di in
    (point_data_dimension_count, point_index_dimension_count, point_num_bytes)
  else
    (point_data_dimension_count, point_data_dimension_count, 0)

let for_file fn =
  let f = Unix.openfile fn [Unix.O_RDONLY] 0 in
  let di = Index_input.from_fd f in
  let index_header = Codec_util.read_header di in
  let version = index_header.version in
  let size = Index_input.read_vint di in
  let rec loop n =
    if n = 0 then
      []
    else begin
      let name = Index_input.read_string di in
      let field_number = Index_input.read_vint di in
      let bits = Index_input.read_byte di in
      let store_term_vector = (bits land mask_store_term_vector) <> 0 in
      let omit_norms = (bits land mask_omit_norms) <> 0 in
      let store_payloads = (bits land mask_store_payloads) <> 0 in
      let is_soft_deletes_field = (bits land mask_soft_deletes_field) <> 0 in
      let index_options = Index_options.from_code (Index_input.read_byte di) in
      let doc_values_type = Doc_values_types.from_code (Index_input.read_byte di) in
      let doc_values_gen = Index_input.read_long di in
      let attributes = Index_input.read_assoc_list_of_strings di in
      let (point_data_dimension_count, point_index_dimension_count, point_num_bytes) = read_point_data version di in
      {
        name;
        field_number;
        store_term_vector;
        omit_norms;
        store_payloads;
        is_soft_deletes_field;
        index_options;
        doc_values_type;
        doc_values_gen;
        attributes;
        point_data_dimension_count;
        point_index_dimension_count;
        point_num_bytes;
      } :: loop (n - 1)
    end in
  let field_infos = loop size in
  Codec_util.check_footer di;
  {
    field_infos;
  }

let get_field { field_infos } n =
  List.find (fun field_info -> field_info.field_number = n) field_infos