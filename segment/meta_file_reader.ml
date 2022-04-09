open Lucene_data_input_2

let segment_suffix = "Lucene84_0"

module Input_type = struct
  type t =
  | Byte1
  | Byte2
  | Byte4
  [@@deriving show, eq]

  let from_code c =
  match c with
  | 0 -> Byte1
  | 1 -> Byte2
  | 2 -> Byte4
  | _ -> failwith "Input type did not match"
end

module Fst_meta = struct
  type t = {
    empty_output: String.t option;
    input_type: Input_type.t;
    start_node: int;
    num_bytes: int;
  } [@@deriving show, eq]
end

module Field_meta = struct
  type t = {
    field_id: int;
    num_terms: int;
    root_code: String.t;
    sum_total_term_freq: int;
    sum_doc_freq: int;
    doc_count: int;
    index_start_fp: int;
    min_term: String.t;
    max_term: String.t;
    fst_meta: Fst_meta.t;
  } [@@deriving show, eq]
end

type t = {
  field_metas: Field_meta.t list;
  index_length: int;
  terms_length: int;
} [@@deriving show, eq]

module Make(Data_input: Data_input.S) = struct
  module Header_reader = Header.Make(Data_input)

  let read_fst_meta ~data_input =
    Header_reader.check_header_exn data_input ~codec_name:"FST" ~min_version:6 ~max_version:7;
    let empty_output = if Data_input.read_byte data_input |> int_of_char = 1 then
      let num_bytes = Data_input.read_vint data_input in
      let arr = Data_input.read_bytes data_input num_bytes in
      Some arr
    else
      None in
    let t = Data_input.read_byte data_input |> int_of_char in
    let input_type = Input_type.from_code t in
    let start_node = Data_input.read_vlong data_input in
    let num_bytes = Data_input.read_vlong data_input in
    {
      Fst_meta.empty_output;
      input_type;
      start_node;
      num_bytes;
    }

  let read ~data_input ~segment_id ~field_infos ~version ~max_doc_count =
    Header_reader.check_index_header_exn data_input
      ~codec_name:"BlockTreeTermsMeta" ~min_version:version ~max_version:version ~expected_id:segment_id ~segment_suffix;
    Header_reader.check_index_header_exn data_input
      ~codec_name:"Lucene84PostingsWriterTerms"  ~min_version:0 ~max_version:1 ~expected_id:segment_id ~segment_suffix;
    let index_block_size = Data_input.read_vint data_input in
    if index_block_size != 128 then failwith "Unsupported block size";
    let num_fields = Data_input.read_vint data_input in
    if num_fields < 0 then failwith "not enough fields";
    let rec loop n =
      if n = 0 then
        []
      else
        let field = Data_input.read_vint data_input in
        let num_terms = Data_input.read_vlong data_input in
        if num_fields <= 0 then failwith "not enough terms";
        let root_code = Data_input.read_string data_input in
        let field_info = Field_infos_reader.get_field field_infos field in
        let sum_total_term_freq = Data_input.read_vlong data_input in
        let sum_doc_freq = if field_info.index_options = Field_infos_reader.Index_options.DOCS then sum_total_term_freq else Data_input.read_vlong data_input in
        let doc_count = Data_input.read_vint data_input in
        if version < 4 then failwith "No support for older indexes";
        let min_term = Data_input.read_string data_input in
        let max_term = Data_input.read_string data_input in
        assert (doc_count >= 0 && doc_count <= max_doc_count); (* Doc count not in range *)
        assert (sum_doc_freq >= doc_count); (* Invalid sum doc freq *)
        assert (sum_total_term_freq >= sum_doc_freq); (* Invalid sum total term freq *)
        let index_start_fp = Data_input.read_vlong data_input in

        let fst_meta = read_fst_meta ~data_input in
        {
          Field_meta.field_id = field;
          num_terms;
          root_code;
          sum_total_term_freq;
          sum_doc_freq;
          doc_count;
          index_start_fp;
          min_term;
          max_term;
          fst_meta;
        }
        ::
        loop (n - 1) in
    let field_metas = loop num_fields in
    let index_length = Data_input.read_long data_input in
    let terms_length = Data_input.read_long data_input in
    {
      field_metas;
      index_length;
      terms_length;
    }
end

let find_field_by_id { field_metas; _ } id =
  List.find (fun { Field_meta.field_id; _ } -> field_id = id) field_metas
