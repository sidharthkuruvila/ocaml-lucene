open Lucene_data_input_2
open Lucene_segment

module Block_terms_dict = Segment_file_reader.Make(Mmapped_file_data_input)

let with_fd filename ~f =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore (f fd);
  Unix.close fd

let segment_file_name = "data/segment.si"

module M = Segment_file_reader.Make(Mmapped_file_data_input)

let segment_file_info = Alcotest.testable Segment_file_reader.pp Segment_file_reader.equal

let test_read () =
  with_fd segment_file_name ~f:(fun fd ->
    let bytes_source = Mmapped_file_bytes.from_fd fd in
    let data_input = Mmapped_file_data_input.of_bytes bytes_source in
    let result = M.read ~data_input in
    let expected = {
      Segment_file_reader.index_header = {
        Header.Index_header.magic = 393008959;
        name = "Lucene86SegmentInfo";
        version = 0;
        object_id = "=\020\221\026\2524\191\141\200\188<\\\151+29";
        suffix_bytes = "";
      };
       version =
        { Header.Lucene_version.major = 134217728; minor = 150994944; bugfix = 0 };
        min_version =
        { Header.Lucene_version.major = 134217728; minor = 150994944; bugfix = 0 };
        doc_count = 65460; is_compound_file = false;
        diagnostic_map =
        [("os", "Mac OS X"); ("java.vendor", "Oracle Corporation");
          ("java.version", "16.0.2"); ("java.vm.version", "16.0.2+7-67");
          ("lucene.version", "8.9.0"); ("os.arch", "x86_64");
          ("java.runtime.version", "16.0.2+7-67"); ("source", "flush");
          ("os.version", "10.15.7"); ("timestamp", "1632532656606")];
        files =
        ["_m.fdm"; "_m.si"; "_m_Lucene84_0.tmd"; "_m.tvm"; "_m.fnm"; "_m.nvm";
          "_m.tvd"; "_m_Lucene84_0.tim"; "_m_Lucene84_0.doc"; "_m.nvd";
          "_m_Lucene84_0.tip"; "_m.fdx"; "_m.tvx"; "_m.fdt"];
        attributes = [("Lucene87StoredFieldsFormat.mode", "BEST_SPEED")];
        sort_fields = []
    } in
    Alcotest.(check segment_file_info) "Segment file info should match" expected result
  )

let tests = [
  "Read a segment file", `Quick, test_read;
]