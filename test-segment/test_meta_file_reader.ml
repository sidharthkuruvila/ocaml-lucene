open Lucene_data_input_2
open Lucene_segment

module Mmapped_file_bytes_source = Bytes_source.Make(Mmapped_file_bytes)
module Mmapped_file_data_input = Data_input.Make(Mmapped_file_bytes_source)

let with_fd filename ~f =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore (f fd);
  Unix.close fd

let segment_file_name = "data/terms_meta.tmd"

module M = Meta_file_reader.Make(Mmapped_file_data_input)

let meta_file_reader = Alcotest.testable Meta_file_reader.pp Meta_file_reader.equal

let field_infos = { Field_infos_reader.field_infos =
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
 has_prox = false; has_payloads = false; has_offsets = false }

let test_read () =
  with_fd segment_file_name ~f:(fun fd ->
    let bytes_source = Mmapped_file_bytes.from_fd fd in
    let data_input = Mmapped_file_bytes_source.of_bytes bytes_source in
    let segment_id = [ 61; 20; 221; 26; 252; 52; 191; 141; 200; 188; 60; 92; 151; 43; 50; 57 ]
      |> List.map char_of_int |> List.to_seq |> String.of_seq in
    let version = 6 in
    let max_doc_count = 70000 in
    let result = M.read ~data_input ~segment_id ~field_infos ~version ~max_doc_count in
    let expected = { Meta_file_reader.field_metas =
    [{ Meta_file_reader.Field_meta.field_id = 5; num_terms = 23065;
      root_code = "\247\2132\0037\209\0039\223\bg\187\011";
      sum_total_term_freq = 420554; sum_doc_freq = 405325; doc_count = 65293;
      index_start_fp = 55; min_term = "1"; max_term = "\197\190ivojin";
      fst_meta =
      { Meta_file_reader.Fst_meta.empty_output =
        (Some "\011\187g\b\2239\003\2097\0032\213\247\r");
        input_type = Meta_file_reader.Input_type.Byte1; start_node = 5542;
        num_bytes = 5543 }
      };
     { Meta_file_reader.Field_meta.field_id = 7; num_terms = 480;
       root_code = "\183\2193\005f\189\bh\193\015j\199\022o\253\028s\215!";
       sum_total_term_freq = 47242; sum_doc_freq = 46269; doc_count = 17168;
       index_start_fp = 5598; min_term = "1895"; max_term = "zoology";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "!\215s\028\253o\022\199j\015\193h\b\189f\0053\219\183\019");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 42;
         num_bytes = 43 }
       };
     { Meta_file_reader.Field_meta.field_id = 0; num_terms = 65460;
       root_code = "\158\150}"; sum_total_term_freq = 65460;
       sum_doc_freq = 65460; doc_count = 65460; index_start_fp = 5641;
       min_term = "1"; max_term = "9999";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output = (Some "}\150\158\003");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 7823;
         num_bytes = 7824 }
       };
     { Meta_file_reader.Field_meta.field_id = 2; num_terms = 6322;
       root_code = "\162\181\132\001"; sum_total_term_freq = 65460;
       sum_doc_freq = 65460; doc_count = 65460; index_start_fp = 13465;
       min_term = "1971-12-01"; max_term = "2021-06-06";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "\001\132\181\162\004");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 1816;
         num_bytes = 1817 }
       };
     { Meta_file_reader.Field_meta.field_id = 4; num_terms = 6322;
       root_code = "\158\210\139\001"; sum_total_term_freq = 65460;
       sum_doc_freq = 65460; doc_count = 65460; index_start_fp = 15282;
       min_term = "1971-12-01"; max_term = "2021-06-06";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "\001\139\210\158\004");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 1894;
         num_bytes = 1895 }
       };
     { Meta_file_reader.Field_meta.field_id = 6; num_terms = 15704;
       root_code = "\147\227\176\001\0049\159\004i\203\007n\249\nw\203\015";
       sum_total_term_freq = 535992; sum_doc_freq = 423272; doc_count = 65380;
       index_start_fp = 17177; min_term = "1";
       max_term = "\215\153\215\148\215\149\215\147\215\148";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "\015\203w\n\249n\007\203i\004\1599\004\001\176\227\147\017");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 4054;
         num_bytes = 4055 }
       };
     { Meta_file_reader.Field_meta.field_id = 3; num_terms = 43456;
       root_code =
       "\211\189\146\002\0042\206\004r\215\006\206\141\n\224\139\016";
       sum_total_term_freq = 382834; sum_doc_freq = 372032; doc_count = 65456;
       index_start_fp = 21232; min_term = "0"; max_term = "\239\189\145";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "\016\139\224\n\141\206\006\215r\004\2062\004\002\146\189\211\017");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 11400;
         num_bytes = 11401 }
       };
     { Meta_file_reader.Field_meta.field_id = 1; num_terms = 7;
       root_code = "\246\236\146\002"; sum_total_term_freq = 65460;
       sum_doc_freq = 65460; doc_count = 65460; index_start_fp = 32633;
       min_term = "Collection"; max_term = "Text";
       fst_meta =
       { Meta_file_reader.Fst_meta.empty_output =
         (Some "\002\146\236\246\004");
         input_type = Meta_file_reader.Input_type.Byte1; start_node = 0;
         num_bytes = 1 }
       }
     ];
     index_length = 756323262421532672; terms_length = 661484886967713792 } in
    Printf.printf "terms meta: %s" (Meta_file_reader.show result);
    Alcotest.(check meta_file_reader) "" expected result
  )

let tests = [
  "Read a terms meta file", `Quick, test_read;
]