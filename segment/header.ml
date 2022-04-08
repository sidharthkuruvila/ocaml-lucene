open Lucene_data_input_2
(*let id_length = 16 *)
let footer_length = 16
let codec_magic = 0x3fd76c17
let int32_mask = 0xFFFFFFFF
let footer_magic = (lnot codec_magic) land int32_mask

module Index_header = struct
  type t = {
    magic: int;
    name: string;
    version: int;
    object_id: string;
    suffix_bytes: string
  } [@@deriving show]
end

module Lucene_version = struct
  type t = {
    major: int;
    minor: int;
    bugfix: int;
  } [@@deriving show]
end


module Check_index_header_errors = struct
  type t =
  | Incorrect_codec_name of { expected: String.t; found: String.t }
  | Unsupported_version of { expected_min: int; expected_max: int; found: int }
  | Incorrect_id of { expected: String.t; found: String.t }
  | Incorrect_segment_suffix of { expected: String.t; found: String.t }
  [@@deriving show]

  let to_string error =
    match error with
    | Incorrect_codec_name { expected; found } ->
      Printf.sprintf "Invalid file header expected '%s' got '%s'" expected found
    | Unsupported_version { expected_min; expected_max; found } ->
      Printf.sprintf "Unsupported expected version in range (%d, %d) got '%d'" expected_min expected_max found
    | Incorrect_id { expected; found } ->
      Printf.sprintf "Incorrect id expected '%s' got '%s'" expected found
    | Incorrect_segment_suffix { expected; found } ->
      Printf.sprintf "Incorrect segment suffix expected '%s' got '%s'" expected found
end

module Make(Data_input: Data_input.S) = struct

  let read_header di =
    let magic = Data_input.read_int di in
    let name = Data_input.read_string di in
    let version = Data_input.read_int di in
    let object_id = Data_input.read_bytes di 16 in
    let suffix_length = Data_input.read_byte di |> int_of_char in
    let suffix_bytes = Data_input.read_bytes di suffix_length in
    { Index_header.magic; name; version; object_id; suffix_bytes }

  let read_lucene_version di =
    let major = Data_input.read_vint di in
    let minor = Data_input.read_vint di in
    let bugfix = Data_input.read_vint di in
    { Lucene_version.major; minor; bugfix }
(*
  let check_header ~codec_name ~min_version ~max_version di =
    let magic = Data_input.read_int di in
    let name = Data_input.read_string di in
    let version = Data_input.read_int di in
    if magic <> codec_magic then failwith "Codec magic does not match";
    if name <> codec_name then failwith (Printf.sprintf "Header name %s did not match codec name %s" name codec_name);
    if min_version > version || version > max_version then failwith "Header version did not match expected"

*)


  let check_footer di = begin
    if Data_input.length di - Data_input.get_position di <> footer_length then
      failwith "Invalid footer length";
    let magic = Data_input.read_int di in
    if magic <> footer_magic then
      failwith "Invalid footer magic"
  end

  let check_index_header data_input ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix =
    let header = read_header data_input in
    let { Index_header.name; version; object_id; suffix_bytes; _ } = header in
    let open Check_index_header_errors in
    if name <> codec_name then
      Some (Incorrect_codec_name { expected = codec_name; found = name })
    else if min_version > version || version > max_version then
      Some (Unsupported_version { expected_min = min_version; expected_max = max_version; found = version })
    else if expected_id <> object_id then
      Some (Incorrect_id { expected = expected_id; found = object_id })
    else if suffix_bytes <> segment_suffix then
      Some (Incorrect_segment_suffix { expected = segment_suffix; found = suffix_bytes })
    else
      None

  let check_index_header_exn data_input ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix =
    check_index_header data_input ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix
    |> Option.iter (fun error -> Printf.printf "Index header check failed: %s" (Check_index_header_errors.to_string error))
end
