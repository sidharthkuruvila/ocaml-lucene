type t = {
  index_header: Codec_util.index_header;
  version: Codec_util.lucene_version;
  min_version: Codec_util.lucene_version option;
  doc_count: int;
  is_compound_file: bool;
  diagnostic_map: (string * string) list;
  files: string list;
  attributes: (string * string) list;
  sort_fields: string list;
}

let read_lucene_version di =
  let major = Data_input.read_int di in
  let minor = Data_input.read_int di in
  let bugfix = Data_input.read_int di in
  { Codec_util.major; minor; bugfix }


let for_file fn =
  let f = Unix.openfile fn [Unix.O_RDONLY] 0 in
  let di = Data_input.from_fd f in
  let index_header = Codec_util.read_header di in
  let version = read_lucene_version di in
  let has_min_version = Data_input.read_byte di = 1 in
  let min_version = if has_min_version then
    Some (read_lucene_version di)
  else
    None in
  let doc_count = Data_input.read_int di in
  let is_compound_file = Data_input.read_byte di = 1 in
  let diagnostic_map = Data_input.read_assoc_list_of_strings di in
  let files = Data_input.read_list_of_strings di in
  let attributes = Data_input.read_assoc_list_of_strings di in
  let sort_fields = Data_input.read_list_of_strings di in
  Codec_util.check_footer di;
  {
    index_header;
    version;
    min_version;
    doc_count;
    is_compound_file;
    diagnostic_map;
    files;
    attributes;
    sort_fields;
  }


