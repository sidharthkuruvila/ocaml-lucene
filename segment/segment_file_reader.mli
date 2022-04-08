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


module Make(Data_input: Data_input.S): sig
  val read: data_input:Data_input.t -> t
  (** [read ~data_input] Reads information about a sgement from a segment (si) file *)
end