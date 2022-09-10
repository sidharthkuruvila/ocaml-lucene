open Lucene_data_input_2


module Make(Data_input: Data_input.S) = struct

  module Lowercase_ascii_compression = Lowercase_ascii_compression.Make_reader(Data_input)
  module Lz4_compression = Lz4_compression.Make_reader(Data_input)

  type t = Data_input.t -> Int.t -> String.t

  let no_decompression di len =
    Data_input.read_bytes di len

  let lowercase_ascii_decompression di len : string =
    Lowercase_ascii_compression.decompress di len

  let lz_74_decompression di length : string =
    Lz4_compression.decompress di length

  let algos = [| no_decompression; lowercase_ascii_decompression; lz_74_decompression |]

  let get_decompression_algo n =
    Array.get algos n
end

