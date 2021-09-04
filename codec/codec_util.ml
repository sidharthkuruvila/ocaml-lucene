open Lucene_data_input

let id_length = 16
let footer_length = 16
let codec_magic = 0x3fd76c17
let int32_mask = 0xFFFFFFFF
let footer_magic = (lnot codec_magic) land int32_mask
type index_header = {
  magic: int;
  name: string;
  version: int;
  object_id: string;
  suffix_bytes: string
}

type lucene_version = {
  major: int;
  minor: int;
  bugfix: int;
}

let read_header di =
  let magic = Index_input.read_int di in
  let name = Index_input.read_string di in
  let version = Index_input.read_uint di in
  let object_id = Index_input.read_bytes di 16 in
  let suffix_length = Index_input.read_byte di in
  let suffix_bytes = Index_input.read_bytes di suffix_length in
  { magic; name; version; object_id; suffix_bytes }

let check_index_header ~codec_name ~min_version ~max_version ~expected_id ~segment_suffix di=
  let header = read_header di in
  if header.name <> codec_name then failwith "Header name did not match codec name";
  if min_version > header.version || header.version > max_version then failwith "Header version did not match expected";
  if expected_id <> header.object_id then failwith "Header id did not match expected";
  if header.suffix_bytes <> segment_suffix then failwith "Header suffix did not match";
  header.version

let check_header ~codec_name ~min_version ~max_version di =
  let magic = Index_input.read_int di in
  let name = Index_input.read_string di in
  let version = Index_input.read_uint di in
  if magic <> codec_magic then failwith "Codec magic does not match";
  if name <> codec_name then failwith (Printf.sprintf "Header name %s did not match codec name %s" name codec_name);
  if min_version > version || version > max_version then failwith "Header version did not match expected"

let read_lucene_version di =
  let major = Index_input.read_vint di in
  let minor = Index_input.read_vint di in
  let bugfix = Index_input.read_vint di in
  { major; minor; bugfix }


let check_footer di = begin
  if Index_input.get_file_length di - Index_input.get_file_pointer di <> footer_length then
    failwith "Invalid footer length";
  let magic = Index_input.read_int di in
  if magic <> footer_magic then
    failwith "Invalid footer magic"
end
