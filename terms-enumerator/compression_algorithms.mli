open Lucene_data_input_2

module Make(Data_input: Data_input.S): sig
  type t = Data_input.t -> Int.t -> String.t
  val get_decompression_algo: Int.t -> Data_input.t -> Int.t -> String.t
end