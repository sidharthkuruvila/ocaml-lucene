open Lucene_utils
open Lucene_data_input


module Encode(Data_output: Data_output.S) = struct
  module Bit_packing = Bit_packing.Encode(Data_output)

  let encode_deltas dout l =
    if l.(0) == 1 && Positive_ints.check_all_equal l then
      Data_output.write_byte dout (char_of_int 0)
    else
      let orred = Array.fold_left (lor) (-1) l in
      let bits = Bit_utils.msb orred in begin
        Data_output.write_byte dout (char_of_int bits);
        Bit_packing.encode (Array.to_list l) bits dout
      end
end

module Decode(Data_input: Data_input.S) = struct
  module Bit_packing = Bit_packing.Decode(Data_input)

  let decode din base =
    let bits_per_value = Data_input.read_byte din in
    if bits_per_value = 0 then
      List.init 128 (fun i -> i + 1 + base)
    else
      failwith "Not implemented yet"
end