open Lucene_data_input_2
module Make(Data_input: Data_input.S) = struct
  type t = int
  type data_input = Data_input.t

  let read = Data_input.read_vint
end