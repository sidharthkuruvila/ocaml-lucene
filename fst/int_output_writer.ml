open Lucene_data_output
module Make(Data_output: Data_output.S) = struct
  type t = int
  type data_input = Data_output.t

  let write = Data_output.write_vint
end