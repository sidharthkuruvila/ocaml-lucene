open Lucene_data_output
module Make(Data_output: Data_output.S) = struct
  type t = string
  type data_output = Data_output.t

  let write = Data_output.write_string
end