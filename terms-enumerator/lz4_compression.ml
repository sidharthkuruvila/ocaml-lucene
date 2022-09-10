open Lucene_data_input_2
open Lucene_data_output

module Make_writer(Data_output: Data_output.S) = struct

  let compress _ _ =
    failwith "Not yet implemented"
end

module Make_reader(Data_input: Data_input.S) = struct

  let read_literal_length token di =
    let length = token lsr 4 in
    if length = 15 then
      let rec loop length =
        let increment = Data_input.read_byte di |> int_of_char in
        let length = length + increment in
        if increment = 255 then
          loop length
        else
          length in
        loop length
    else
      length

  let read_match_length token di =
    let length = token land 0x0F in
    (if length = 15 then
      let rec loop length =
        let increment = Data_input.read_byte di |> int_of_char in
        let length = length + increment in
        if increment = 255 then
          loop length
        else
          length in
        loop length
    else
      length) + 4

  let read_offset di =
    let b1 = Data_input.read_byte di |> int_of_char in
    let b2 = Data_input.read_byte di |> int_of_char in
    b1 lor (b2 lsl 8)

  let decompress di uncompressed_length =
    let buf = Buffer.create uncompressed_length in
    let rec loop () =
      if (Buffer.length buf) < uncompressed_length then
        let token = Data_input.read_byte di |> int_of_char in
        let literal_length = read_literal_length token di in
        let literal = Data_input.read_bytes di literal_length in
        Buffer.add_string buf literal;
        if (Buffer.length buf) < uncompressed_length then begin
          let offset = read_offset di in
          let match_length = read_match_length token di in
          let s = Buffer.sub buf ((Buffer.length buf) - offset) match_length in
          Buffer.add_string buf s;
          loop () end in
    loop ();
    Buffer.contents buf
end