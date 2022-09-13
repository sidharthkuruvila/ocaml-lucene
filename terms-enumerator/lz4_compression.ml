open Lucene_data_input_2
open Lucene_data_output

module Make_writer(Data_output: Data_output.S) = struct

  let min_match = 4
  let last_literals = 5
  
  let hash i ~hash_bits =
    ((i * 2654435761) land 0xffffffff) lsr (32 - hash_bits)

  let hash16 = hash ~hash_bits:16
    
  let read_int s i =
    assert (i + 3 < String.length s);
    let get_byte s i = String.get s i |> int_of_char in
    (get_byte s i lsl 24)
    lor (get_byte s (i + 1) lsl 16)
    lor (get_byte s (i + 2) lsl 8)
    lor (get_byte s (i + 3))
    
  let common s n1 n2 =
    let sl = String.length s in
    let rec loop i =
      let i1 = n1 + i in
      let i2 = n2 + i in
      if sl > i2 && sl > i2 then
        if Char.equal (String.get s i1) (String.get s i2) then
          loop (i + 1)
        else
          i
      else
        i in
    loop 0


  module Segment = struct
    type t =
      | Segment of {
          start: Int.t;
          literal_length: Int.t;
          match_source: Int.t;
          match_length: Int.t;
        }
      | Last_segment of {
          start: Int.t;
          literal_length: Int.t;
        }
  end
    
  let read_segment s segment_start ~table =
    let ls = String.length s in
    let rec loop i =
      if i > ls - (min_match + last_literals) then
        (ls, Segment.Last_segment {start = segment_start; literal_length = (ls - segment_start)})
      else
        let n = read_int s i in
        let hn = hash16 n in
        let match_source = Array.get table hn in
        Array.set table hn i;
        if match_source > -1 && i - match_source < 65535 then
          let common_length = common s match_source i in
          if common_length >= min_match then
            let match_length = Int.min common_length (ls - last_literals - i) in
            (i + match_length, Segment.Segment {
                                   start = segment_start;
                                   literal_length = i;
                                   match_source;
                                   match_length
            })
          else
            loop (i + 1)
        else
          loop (i + 1) in
    loop segment_start

  let write_overflow_bytes data_output n =
    if n > 14 then
      let overflow = n - 15 in
      let rec loop n = 
        if n > 254 then
          begin
            Data_output.write_byte data_output (char_of_int 255);
            loop (n - 255)
          end
        else
          Data_output.write_byte data_output (char_of_int n) in
      loop overflow

  let write_int16_le data_output n =
    let b1 = n land 0xff in
    let b2 = n lsr 8 in
    Data_output.write_byte data_output (char_of_int b1);
    Data_output.write_byte data_output (char_of_int b2)
      
  let write_segment data_output s segment =
    let get_nibble n = if n < 15 then n else 15 in
    match segment with
    | Segment.Segment { start; literal_length; match_source; match_length } ->
       let match_dest = start + literal_length in
       let literal = String.sub s start literal_length in
       let match_offset = match_dest - match_source in
       let match_length_shrunk = match_length - 4 in
       let literal_length_nibble = get_nibble literal_length in
       let match_length_nibble = get_nibble match_length_shrunk in
       let token = (literal_length_nibble lsl 4) lor match_length_nibble in
       Printf.printf "token: %X\n" token; 
       Data_output.write_byte data_output (char_of_int token);
       write_overflow_bytes data_output literal_length;
       Data_output.write_bytes data_output literal;
       write_int16_le data_output match_offset;
       write_overflow_bytes data_output match_length_shrunk
    | Segment.Last_segment { start; literal_length } ->
       let literal_length_nibble = get_nibble literal_length in
       let token = literal_length_nibble lsl 4 in
       let literal = String.sub s start literal_length in
       Data_output.write_byte data_output (char_of_int token);
       write_overflow_bytes data_output literal_length;
       Data_output.write_bytes data_output literal

    
         
  let compress data_output s =
    let ls = String.length s in
    let table = Array.make 65535 (-1) in
    let rec loop segment_start =
      if segment_start < ls then
        begin
          let (next_segment_start, segment) = read_segment s segment_start ~table in
          write_segment data_output s segment;
          loop (next_segment_start)
        end in
    loop 0
    
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
        Printf.printf "token: %X\n" token;
        let literal_length = read_literal_length token di in
        let literal = Data_input.read_bytes di literal_length in
        Printf.printf "literal: %s\n" literal;
        Buffer.add_string buf literal;
        if (Buffer.length buf) < uncompressed_length then begin
          let offset = read_offset di in
          let match_length = read_match_length token di in
          let s = Buffer.sub buf ((Buffer.length buf) - offset) match_length in
          Printf.printf "offset: %d, match_length: %d, match: %s\n"
            offset match_length s;
          Buffer.add_string buf s;
          loop () end in
    loop ();
    Buffer.contents buf
end
