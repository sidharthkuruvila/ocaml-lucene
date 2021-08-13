module Input_type = struct
  type t =
  | Byte1
  | Byte2
  | Byte4

  let from_code c =
  match c with
  | 0 -> Byte1
  | 1 -> Byte2
  | 2 -> Byte4
  | _ -> failwith "Input type did not match"
end


type t = {
  empty_output: (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t option;
  input_type: Input_type.t;
  start_node: Int64.t;
  num_bytes: Int64.t;
  index_in: Data_input.t;
  index_file_pointer: int;
}




let read ~meta_in ~index_in =
  Codec_util.check_header ~codec_name:"FST" ~min_version:6 ~max_version:7 meta_in;
  let empty_output = if Data_input.read_byte meta_in = 1 then
    let num_bytes = Data_input.read_vint meta_in in
    let arr = Data_input.read_byte_array meta_in num_bytes in
    Some arr
  else
    None in
  let t = Data_input.read_byte meta_in in
  let input_type = Input_type.from_code t in
  let start_node = Data_input.read_vlong meta_in in
  let num_bytes = Data_input.read_vlong meta_in in
  let index_file_pointer = Data_input.get_file_pointer index_in in
  {
    empty_output;
    input_type;
    start_node;
    num_bytes;
    index_in;
    index_file_pointer;
  }

