open Lucene_data_input


let terms_codec = "Lucene84PostingsWriterTerms"
let doc_codec = "Lucene84PostingsWriterDoc"
let pos_codec = "Lucene84PostingsWriterPos"
let pay_codec = "Lucene84PostingsWriterPay"

let version_start = 0
let version_current = 1

let make_segment_file_name segment_name segment_suffix =
  Printf.sprintf "%s_Lucene84_0.%s" segment_name segment_suffix


type t = {
  doc_in: Index_input.t;
  pos_in: Index_input.t option;
  pay_in: Index_input.t option;
}

let create { Segment_read_state.dir; segment_info; field_infos} =
  let segment_suffix = "Lucene84_0" in
  let segment_name = segment_info.Segment_info.name in
  let doc_file_name = make_segment_file_name segment_name "doc" in
  let doc_in = Directory.open_input dir doc_file_name in
  let version = Codec_util.check_index_header ~codec_name:doc_codec ~min_version:version_start ~max_version:version_current ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix doc_in in
  (* CodecUtil.retrieveChecksum(posIn); *)
  let pos_in = if field_infos.Field_infos.has_prox then
    let pos_file_name = make_segment_file_name segment_name "pos" in
    let pos_in = Directory.open_input dir pos_file_name in
    ignore (Codec_util.check_index_header ~codec_name:pos_codec ~min_version:version ~max_version:version ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix doc_in);
    (* CodecUtil.retrieveChecksum(posIn); *)
    Some pos_in
  else
    None in
    let pay_in = if field_infos.Field_infos.has_prox then
      let pos_file_name = make_segment_file_name segment_name "pay" in
      let pos_in = Directory.open_input dir pos_file_name in
      ignore (Codec_util.check_index_header ~codec_name:pay_codec ~min_version:version ~max_version:version ~expected_id:segment_info.Segment_info.seg_id ~segment_suffix doc_in);
      (* CodecUtil.retrieveChecksum(posIn); *)
      Some pos_in
    else
      None in
  {
    doc_in;
    pos_in;
    pay_in;
  }

(*let postings ~field_info ~term_state ~flags =
  let index_has_positions = Field_infos.has_positions field_info in
*)