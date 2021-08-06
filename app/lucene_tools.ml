open Lucene_codec

let _ =
  let f = "/Users/sidharthkuruvila/src/lucene-playground/app/test-index/segments_10" in
  let segments = Segments.for_file f in
  let lucene_version = segments.lucene_version in
  let version = segments.version in
  let counter = segments.name_counter in
  let size = segments.seg_count in
  let ms_lucene_version = segments.ms_lucene_version in
  Printf.printf "major: %d; minor: %d; bugfix: %d\n"  lucene_version.major lucene_version.minor lucene_version.bugfix;
  Printf.printf "version: %s; counter: %s; size: %d\n" (Int64.to_string version) (Int64.to_string counter) size;
  Printf.printf "MS - major: %d; minor: %d; bugfix: %d\n"  ms_lucene_version.major ms_lucene_version.minor ms_lucene_version.bugfix

