open Lucene_codec
let dir = "/Users/sidharthkuruvila/src/lucene-playground/app/catalog-index"

let find_field field_name ~field_readers =
  List.find (fun (_, fr, _) -> fr.Field_reader.field_info.name = field_name) field_readers

let _ =
  let segments = Segments.latest dir in
  List.iter (fun segment ->
    let prefix = segment.Segments.Segment.seg_name in
    let seg_id = segment.seg_id in
    let segment_info = Segment_info.read dir prefix seg_id in
    let field_infos = Field_infos.read dir prefix in
    let segment_read_state = Segment_read_state.make ~dir ~segment_info ~field_infos in
    let (block_tree_terms_reader, field_readers) = (Block_tree_terms_reader.create segment_read_state) in
    let _ = Lucene_84_postings_reader.create segment_read_state in
    (*List.iter (fun (_, fi) -> print_endline fi.Field_reader.field_info.name) field_readers;*)
    let (_, field_reader, fst) = find_field "title" ~field_readers in
    print_endline "in hee";
    ignore (Terms_enumerator.seek_exact ~block_tree_terms_reader ~field_reader ~fst "abc")
  ) segments.segments

