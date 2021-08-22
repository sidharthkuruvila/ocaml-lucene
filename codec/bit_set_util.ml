
let is_bit_set ~get_byte_at index =
  let byte = get_byte_at (index lsr 3) in
  Printf.printf "Byte = %x\n" byte;
  let bit_index = index land 7 in
  byte land (1 lsl bit_index) != 0