
let is_bit_set ~get_byte_at index =
  let byte = get_byte_at (index lsr 3) in
  Printf.printf "Byte = %x\n" byte;
  let bit_index = index land 7 in
  byte land (1 lsl bit_index) != 0


let mask2 =  0x55
let mask4 =  0x33
let mask8 =  0x0F
let count_byte_bits n =
  let count2 = ((n lsr 1) land mask2) + (n land mask2) in
  let count4 = ((count2 lsr 2) land mask4) + (count2 land mask4) in
  let count8 = ((count4 lsr 4) land mask8) + (count4 land mask8) in
  count8

let range n =
  Seq.unfold (fun i -> if i < n then Some (i, i + 1) else None) 0

let count_bits_upto ~get_byte_at index =
  let byte_index = index lsr 3 in
  let byte_range = range byte_index in
  let byte_counts = Seq.map (fun n ->
    let byte = get_byte_at n in
    count_byte_bits byte) byte_range in
  let bytes_count = Seq.fold_left (fun a b -> a + b) 0 byte_counts in
  let remainder = index mod 8 in
  let mask = 1 lsl remainder - 1 in
  bytes_count + (count_byte_bits (get_byte_at byte_index land mask))

let count_bits ~get_byte_at byte_count =
  let byte_range = range byte_count in
  let byte_counts = Seq.map (fun n ->
    let byte = get_byte_at n in
    count_byte_bits byte) byte_range in
  Seq.fold_left (fun a b -> a + b) 0 byte_counts