open Lucene_data_input_2


type t = {
  index_header: Header.Index_header.t;
  version: Header.Lucene_version.t;
  min_version: Header.Lucene_version.t;
  doc_count: int;
  is_compound_file: bool;
  diagnostic_map: (string * string) list;
  files: string list;
  attributes: (string * string) list;
  sort_fields: string list;
} [@@deriving show, eq]

module Make(Data_input: Data_input.S) = struct
  module Header_reader = Header.Make(Data_input)
  let read_lucene_version di =
    let major = Data_input.read_int di in
    let minor = Data_input.read_int di in
    let bugfix = Data_input.read_int di in
    { Header.Lucene_version.major; minor; bugfix }

  let read ~data_input =
    let index_header = Header_reader.read_header data_input in
    let version = read_lucene_version data_input in
    let has_min_version = Data_input.read_byte data_input |> int_of_char = 1 in
    let min_version = if has_min_version then
      read_lucene_version data_input
    else
      version in

    let doc_count = Data_input.read_uint data_input in
    let is_compound_file = Data_input.read_byte data_input |> int_of_char = 1 in
    let diagnostic_map = Data_input.read_assoc_list_of_strings data_input in
    let files = Data_input.read_list_of_strings data_input in
    let attributes = Data_input.read_assoc_list_of_strings data_input in
    let sort_fields = Data_input.read_list_of_strings data_input in
    Header_reader.check_footer data_input;
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
end