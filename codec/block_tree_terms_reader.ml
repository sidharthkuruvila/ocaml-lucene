type t = {
  terms_in: Index_input.t;
  index_in: Index_input.t;
}

let make_segment_file_name segment_name segment_suffix =
  Printf.sprintf "%s_Lucene84_0.%s" segment_name segment_suffix

let create { Segment_read_state.dir; segment_info; field_infos} =
  let segment_suffix = "Lucene84_0" in
  let segment_name = segment_info.Segment_info.name in
  let terms_file_name = make_segment_file_name segment_name "tim" in
  let terms_input = Directory.open_input dir terms_file_name in
  let version = Codec_util.check_index_header ~codec_name:"BlockTreeTermsDict" ~min_version:3 ~max_version:6 ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix terms_input in
  let index_file_name = make_segment_file_name segment_name "tip" in
  let index_input = Directory.open_input dir index_file_name in
  ignore (Codec_util.check_index_header ~codec_name:"BlockTreeTermsIndex" ~min_version:version ~max_version:version ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix index_input);
  Printf.printf "Version: %d\n" version;
  if version < 6 then failwith "We only support versions with a separate meta file";
  let meta_name = make_segment_file_name segment_name "tmd" in
  Directory.open_input_with dir meta_name ~f:(fun meta_input ->
  ignore (Codec_util.check_index_header ~codec_name:"BlockTreeTermsMeta"  ~min_version:version ~max_version:version ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix meta_input);
  ignore (Codec_util.check_index_header ~codec_name:"Lucene84PostingsWriterTerms"  ~min_version:0 ~max_version:1 ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix meta_input);
  let index_block_size = Index_input.read_vint meta_input in
  if index_block_size != 128 then failwith "Unsupported block size";
  let num_fields = Index_input.read_vint meta_input in
  if num_fields < 0 then failwith "not enough fields";
  let block_tree_terms_reader = {
    terms_in = terms_input;
    index_in = index_input;
  } in
  let rec loop n =
    if n = 0 then
     []
    else
      let field = Index_input.read_vint meta_input in
      let num_terms = Int64.to_int (Index_input.read_vlong meta_input) in
      if num_fields <= 0 then failwith "not enough terms";
      let root_code = Index_input.read_string meta_input in
      let field_info = Field_infos.get_field field_infos field in
      let sum_total_term_freq = Index_input.read_vlong meta_input in
      let sum_doc_freq = if field_info.index_options = Field_infos.Index_options.DOCS then sum_total_term_freq else Index_input.read_vlong meta_input in
      let doc_count = Index_input.read_vint meta_input in
      if version < 4 then failwith "No support for older indexes";
      let min_term = Index_input.read_string meta_input in
      let max_term = Index_input.read_string meta_input in
      if doc_count < 0 || doc_count > segment_info.doc_count then failwith "doc count not in range";
      if sum_doc_freq < (Int64.of_int doc_count) then failwith "Invalid sum doc freq";
      if sum_total_term_freq < sum_doc_freq then failwith "Invalid sum total term freq";
      let index_start_fp = Index_input.read_vlong meta_input in
      let index_input_clone = Index_input.copy index_input in
      Index_input.set_file_pointer index_input_clone (Int64.to_int index_start_fp);
(*    Printf.printf "index start fp = %d\n" (Int64.to_int index_start_fp);*)
      let _ = Fst.read ~meta_in:meta_input ~index_in:index_input_clone in
      (field, {
        Field_reader.field_info: Field_infos.field_info;
        num_terms;
        root_code;
        sum_total_term_freq;
        sum_doc_freq;
        doc_count;
        index_start_fp;
        (*This one has been closed so it shouldn't work, right?*)
        meta_in = meta_input;
        index_in = index_input;
        min_term;
        max_term;
      })
      ::
      loop (n - 1) in
  let field_readers = loop num_fields in
  let index_length = Index_input.read_long meta_input in
  let terms_length = Index_input.read_long meta_input in
  Printf.printf "index length %d, terms_length %d" (Int64.to_int index_length) (Int64.to_int terms_length);

  (block_tree_terms_reader, field_readers))
