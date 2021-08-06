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
  let magic = Data_input.read_int di in
  let name = Data_input.read_string di in
  let version = Data_input.read_uint di in
  let object_id = Data_input.read_bytes di 16 in
  let suffix_length = Data_input.read_byte di in
  let suffix_bytes = Data_input.read_bytes di suffix_length in
  { magic; name; version; object_id; suffix_bytes }

let read_lucene_version di =
  let major = Data_input.read_vint di in
  let minor = Data_input.read_vint di in
  let bugfix = Data_input.read_vint di in
  { major; minor; bugfix }


let check_footer di = begin
  if Data_input.get_file_length di - Data_input.get_file_pointer di <> footer_length then
    failwith "Invalid footer length";
  let magic = Data_input.read_int di in
  if magic <> footer_magic then
    failwith "Invalid footer magic"
end
