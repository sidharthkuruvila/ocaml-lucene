open Lucene_data_input_2
open Lucene_data_output

module Make_writer(Data_output: Data_output.S): sig
  val compress: Data_output.t -> String.t -> unit
end

module Make_reader(Data_input: Data_input.S): sig
  val decompress: Data_input.t -> Int.t -> String.t
end