let move_bits dest_num dest_bits src_num src_bits =
  let movable_bits = min dest_bits src_bits in
  let bits_to_move = src_num lsr (src_bits - movable_bits) in
  let dest_num = (dest_num lsl movable_bits) lor bits_to_move in
  let src_num = src_num land ((1 lsl (src_bits - movable_bits)) - 1) in
  (dest_num, dest_bits - movable_bits, src_num, src_bits - movable_bits)

let repack_bits l original_bit_count repacked_bit_count =
  let rec pack_next_int l byte_to_fill bits_to_fill =
    match l with
    | n::rest -> pack rest byte_to_fill bits_to_fill n original_bit_count
    | [] when bits_to_fill != repacked_bit_count -> [byte_to_fill]
    | [] -> []
  and pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack =
    if bits_to_pack = 0 then
           pack_next_int l byte_to_fill bits_to_fill
    else if bits_to_fill = 0 then
      byte_to_fill::(pack l 0 repacked_bit_count num_to_pack bits_to_pack)
    else
      let (byte_to_fill, bits_to_fill, num_to_pack, bits_to_pack)
        = move_bits byte_to_fill bits_to_fill num_to_pack bits_to_pack in
      pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack in
  pack_next_int l 0 repacked_bit_count


(* Encode a list of numbers each using max num_size bits
  The output will be packed into 8 bit nums.
  *)
let encode l num_size = repack_bits l num_size 8

(* Decode a list of bit packed numbers 8 bit numbers.
  The output will be unpacked into num_size ints.
  *)
let decode l num_size = repack_bits l 8 num_size



