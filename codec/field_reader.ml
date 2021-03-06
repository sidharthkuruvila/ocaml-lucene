open Lucene_data_input

type t = {
  field_info: Field_infos.Field_info.t;
  num_terms: int;
  root_code: string;
  sum_total_term_freq: Int64.t;
  sum_doc_freq: Int64.t;
  doc_count: int;
  index_start_fp: Int64.t;
  (*This one has been closed so it shouldn't work, right?*)
  meta_in: Index_input.t;
  index_in: Index_input.t;
  min_term: string;
  max_term: string;
}

let get_size field_reader = field_reader.num_terms