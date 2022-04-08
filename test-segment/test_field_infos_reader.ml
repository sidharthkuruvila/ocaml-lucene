open Lucene_data_input_2
open Lucene_segment

module Mmapped_file_bytes_source = Bytes_source.Make(Mmapped_file_bytes)
module Mmapped_file_data_input = Data_input.Make(Mmapped_file_bytes_source)

module Block_terms_dict = Segment_file_reader.Make(Mmapped_file_data_input)

let with_fd filename ~f =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore (f fd);
  Unix.close fd

let segment_file_name = "data/field_infos.fnm"

module M = Field_infos_reader.Make(Mmapped_file_data_input)

let field_infos = Alcotest.testable Field_infos_reader.pp Field_infos_reader.equal

let test_read () =
  with_fd segment_file_name ~f:(fun fd ->
    let bytes_source = Mmapped_file_bytes.from_fd fd in
    let data_input = Mmapped_file_bytes_source.of_bytes bytes_source in
    let result = M.read ~data_input in
    let expected = { Field_infos_reader.field_infos =
                     [{ Field_infos_reader.Field_info.name = "id"; field_number = 0;
                        store_term_vector = false; omit_norms = true; store_payloads = false;
                        is_soft_deletes_field = false;
                        index_options = Field_infos_reader.Index_options.DOCS;
                        doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                        doc_values_gen = -1;
                        attributes =
                        [("PerFieldPostingsFormat.format", "Lucene84");
                          ("PerFieldPostingsFormat.suffix", "0")];
                        point_data_dimension_count = 0; point_index_dimension_count = 0;
                        point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "type"; field_number = 1;
                         store_term_vector = false; omit_norms = true; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "issued_date"; field_number = 2;
                         store_term_vector = false; omit_norms = true; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "title"; field_number = 3;
                         store_term_vector = true; omit_norms = false; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS_AND_FREQS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "language"; field_number = 4;
                         store_term_vector = false; omit_norms = true; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "authors"; field_number = 5;
                         store_term_vector = true; omit_norms = false; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS_AND_FREQS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "subjects"; field_number = 6;
                         store_term_vector = true; omit_norms = false; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS_AND_FREQS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 };
                       { Field_infos_reader.Field_info.name = "bookshelves"; field_number = 7;
                         store_term_vector = true; omit_norms = false; store_payloads = false;
                         is_soft_deletes_field = false;
                         index_options = Field_infos_reader.Index_options.DOCS_AND_FREQS;
                         doc_values_type = Field_infos_reader.Doc_values_types.NONE;
                         doc_values_gen = -1;
                         attributes =
                         [("PerFieldPostingsFormat.format", "Lucene84");
                           ("PerFieldPostingsFormat.suffix", "0")];
                         point_data_dimension_count = 0; point_index_dimension_count = 0;
                         point_num_bytes = 0 }
                       ];
                     has_prox = false; has_payloads = false; has_offsets = false } in
    Alcotest.(check field_infos) "Field infos should match" expected result
  )

let tests = [
  "Read field infos", `Quick, test_read;
]