open Lucene_data_input_2
open Lucene_data_output

module Make_writer(Data_output: Data_output.S) = struct

  let shrink_char byte =
    let b = byte + 1 in
    (b land 0x1F) lor (b land 0x40) lsr 1

  let shrink s =
    String.map (fun c -> char_of_int (shrink_char (int_of_char c))) s

   let pack data_output s = begin
     let len = String.length s in
     let packable_bytes_length = len / 4 in
     let packable_bytes_start = len - packable_bytes_length in
     for i = 0 to packable_bytes_length * 3 - 1 do
       let shift_index = i mod packable_bytes_length in
       let packable_bits_index = packable_bytes_start + shift_index in
       let packable_bits_char = String.get s packable_bits_index in
       let packable_bits = int_of_char packable_bits_char lsr (4 - i/packable_bytes_length * 2) land 3 in
       let shrunk_char = String.get s i |> int_of_char in
       let updated_char = shrunk_char + (packable_bits lsl 6) in
       Data_output.write_byte data_output (char_of_int updated_char)
     done;
     for i = packable_bytes_length * 3 to packable_bytes_start - 1 do
       Data_output.write_byte data_output (String.get s i)
     done
   end

  let is_compressable c =
    (0x1F < c && c <= 0x3F) || (0x5F < c && c <= 0x7F)

  let exception_count s =
    let rec loop i j count =
      if i = String.length s then
        count
      else if not (is_compressable (int_of_char (String.get s i))) then
        loop (i + 1) 1 (count + (j / 0xFF) + 1)
      else
        loop (i + 1) (j + 1) count in
    loop 0 0 0

  let exceptions data_output s =
    let rec loop i j =
      if i < String.length s then
        let c = int_of_char (String.get s i) in
        if j = 255 || not (is_compressable c) then begin
          Data_output.write_byte data_output (char_of_int j);
          Data_output.write_byte data_output (char_of_int c);
          loop (i + 1) 1
          end
        else
          loop (i + 1) (j + 1) in
    loop 0 0

  let compress data_output s =
    let shrunk = shrink s in
    let exc = exception_count s in
    pack data_output shrunk;
    Data_output.write_vint data_output exc;
    exceptions data_output s
end

module Make_reader(Data_input: Data_input.S) = struct
  let unshrink_char b =
    (((b land 0x20) lsl 1) lor 0x20 lor (b land 0x1F)) - 1

  let decompress di uncompressed_length =
    let packed_bytes_length = uncompressed_length / 4 in
    let compressed_bytes_length = uncompressed_length - packed_bytes_length in
    let compressed_bytes = Data_input.read_bytes di compressed_bytes_length in
    let bytes = Bytes.create uncompressed_length in
    Bytes.blit_string compressed_bytes 0 bytes 0 compressed_bytes_length;
    for i = 0 to packed_bytes_length - 1 do
      let c1 = Bytes.get bytes i |> int_of_char in
      let c2 = Bytes.get bytes (packed_bytes_length + i) |> int_of_char in
      let c3 = Bytes.get bytes (2 * packed_bytes_length + i) |> int_of_char in
      let c = (c3 lsr 6) lor ((c2 land 0xC0) lsr 4) lor ((c1 land 0xC0) lsr 2) in
      Bytes.set bytes (compressed_bytes_length + i) (char_of_int c)
    done;
    for i = 0 to (Bytes.length bytes - 1) do
      Bytes.set bytes i (unshrink_char (Bytes.get bytes i |> int_of_char) |> char_of_int)
    done;
    let len = Data_input.read_byte di |> int_of_char in
    if len > 0 then begin
        let prev_exception = ref 0 in
        for _ = 1 to len do
          let i = Data_input.read_byte di  |> int_of_char in
          prev_exception := !prev_exception + i;
          let b = Data_input.read_byte di  in
          Bytes.set bytes !prev_exception b
        done
      end;
    Bytes.to_string bytes
end