open Lucene_data_input

let no_decompression di len =
  Index_input.read_bytes di len

let lowercase_ascii_decompression (_:Index_input.t) (_:int) : string =
  failwith "Lowercase ascii compression not implemented yet"

let lz_74_decompression (_:Index_input.t) (_:int) : string =
  failwith "Lowercase ascii compression not implemented yet"

let algos = [| no_decompression; lowercase_ascii_decompression; lz_74_decompression |]

let get_decompression_algo n =
  Array.get algos n