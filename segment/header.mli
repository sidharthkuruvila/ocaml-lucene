open Lucene_data_input_2


module Index_header: sig
  type t [@@deriving show]
end

module Lucene_version: sig
  type t [@@deriving show]
end

module Check_index_header_errors: sig
  type t [@@deriving show]
  (** The type representing index header validation errors *)

  val to_string: t -> String.t
  (** [to_string error] returns a string representation of an error *)
end


module Make(Data_input: Data_input.S): sig
  val read_header: Data_input.t -> Index_header.t
  (** [reader_header data_input] Read an index header from a data input *)

  val check_index_header: Data_input.t -> codec_name:String.t
    -> min_version: Int.t -> max_version: Int.t -> expected_id: String.t ->  segment_suffix: String.t
    -> Check_index_header_errors.t option
  (** [check_index_header header ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix] validates
   the contends of an index header *)

  val check_index_header_exn: Data_input.t -> codec_name:String.t
    -> min_version: Int.t -> max_version: Int.t -> expected_id: String.t ->  segment_suffix: String.t
    -> unit
  (** [check_index_header header ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix] checks if the
   index header is valid

   It throws an exception if the contents of an index header are invalid. *)

  val check_footer: Data_input.t -> unit
  (** [check_footer data_input] checks if the footer is valid.

   It throws an exception if the footer is invalid. *)

  val read_lucene_version: Data_input.t -> Lucene_version.t
  (** [read_lucene_version data_input] reads the lucene version *)
end
