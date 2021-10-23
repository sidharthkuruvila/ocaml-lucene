open Lucene_data_input


let binary64 n =
  let open Int64 in
  let bytes = Bytes.make 64 '0' in
  let rec loop i =
    let c = if logand (shift_left 1L i) n = 0L then
      '0'
    else
      '1' in
    Bytes.set bytes (63 - i) c;
    if i < 63 then loop (i + 1) in
  loop 0;
  Bytes.to_string bytes

let grouped group_size l  =
  let rec loop l fill lol n =
    match l with
    | [] -> List.rev ((List.rev fill) :: lol)
    | _ when n = 0 ->
        loop l [] ((List.rev fill)::lol) group_size
    | x::rest ->
        loop rest (x::fill) lol (n - 1) in
  loop l [] [] group_size

let zip_l l =
  let rec firsts l fs rs=
    match l with
    | [] -> Some (List.rev fs, List.rev rs)
    | (f::r)::rest -> firsts rest (f::fs) (r::rs)
    | _ -> None in
  let rec loop l =
    match firsts l [] [] with
    | None -> []
    | Some(row, rest) -> row::(loop rest) in
  loop l


let make_masks width =
  let rec make_init parts acc =
    if parts = 0 then acc
    else make_init (parts - 1) (Int64.logor (Int64.shift_left acc width) 1L) in
  let parts = 64/width in
  let init = make_init parts 0L in
  let rec loop n acc =
    if n = 0 then
      [0L]
    else
      acc::(loop (n - 1) (Int64.logor (Int64.shift_left acc 1) init)) in
  let l = loop width init in
  let a = Array.of_list l in
  fun i -> a.(i)

let masks8 = make_masks 8
let masks16 = make_masks 16
let masks32 = make_masks 32

let rec take_n l n =
  if n = 0 then
    ([], l)
  else
    match l with
    | x::rest ->
      let (l1, l2) = take_n rest (n - 1) in
      (x::l1, l2)
    | _ -> failwith "taken expects at least n items"

let rec fill n v =
  if n = 0 then []
  else v::(fill (n - 1) v)

let rec zeros n =
  if n = 0 then []
  else 0L::(zeros (n - 1))

let repack_bits l mask original_bit_count repacked_bit_count =
  let move_bits dest_num dest_bits src_num src_bits =
    let movable_bits = min dest_bits src_bits in
    let bits_to_move = Int64.shift_right_logical src_num (src_bits - movable_bits) in
    let dest_num = Int64.logor (Int64.shift_left dest_num  movable_bits) bits_to_move in
    let dest_num = Int64.logand dest_num (mask (dest_bits - 1)) in
    (dest_num, dest_bits - movable_bits, src_num, src_bits - movable_bits) in
  let rec pack_next_int l byte_to_fill bits_to_fill =
    match l with
    | n::rest -> pack rest byte_to_fill bits_to_fill n original_bit_count
    | [] when bits_to_fill != repacked_bit_count -> [byte_to_fill]
    | [] -> []
  and pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack =
    if bits_to_pack = 0 then
           pack_next_int l byte_to_fill bits_to_fill
    else if bits_to_fill = 0 then
      byte_to_fill::(pack l 0L repacked_bit_count num_to_pack bits_to_pack)
    else
      let (byte_to_fill, bits_to_fill, num_to_pack, bits_to_pack)
        = move_bits byte_to_fill bits_to_fill num_to_pack bits_to_pack in
      pack l byte_to_fill bits_to_fill num_to_pack bits_to_pack in
  pack_next_int l 0L repacked_bit_count


let pack_long bits l =
  let open Int64 in
  let rec loop l acc =
    match l with
    | [] -> acc
    | n::rest -> loop rest (logor (shift_left acc bits) (of_int n)) in
  loop l 0L

let pack_longs_n bits_per_num l =
  let len = List.length l in
  assert (len * bits_per_num mod 64 = 0);
  let longs_len = len * bits_per_num / 64 in
  let l = l |> grouped longs_len |> zip_l in
  List.map (pack_long bits_per_num) l
  (*let rec loop l =
    match l with
    | parts::rest ->
       (pack_long bits_per_num parts)::(loop rest)
    | [] -> [] in
  loop l*)

let unpack_long bits l =
  let open Int64 in
  let mask = sub (shift_left 1L bits) 1L in

  let num_count = 64 / bits in
  let rec loop shift =
    if shift < 0 then
      []
    else
      to_int (logand (shift_right_logical l shift) mask) :: loop (shift - bits) in
  loop ((num_count - 1) * bits)

let unpack_longs_n bits_per_num l =
  List.concat (zip_l (List.map (unpack_long bits_per_num) l))




(* Encode a list of numbers each using max num_size bits.

  *)
let pack l num_size =
  let item_count = List.length l in
  let pack_n l width masks =
    let encoded_length = (item_count*num_size)/64 in
    let encoded = zeros encoded_length in
    let shift = width - num_size in
    let rec loop encoded shift l=
      if shift < 0 then
        (encoded, shift + num_size, l)
      else begin
        let (l, rest) = take_n l encoded_length in
        let encoded = List.map2 (fun e n ->
          let open Int64 in
          logor e (shift_left n shift)
        ) encoded l in
        loop encoded (shift - num_size) rest end in
    let (encoded, shift, l) = loop encoded shift l in
    if shift > 0 then
      let repacked_bits = repack_bits l masks num_size shift in
      let encoded = List.map2 (fun e n -> Int64.logor e n) encoded repacked_bits in
      encoded
    else
      encoded in
  if num_size <= 8 then
    let width = 8 in
    let l = pack_longs_n 8 l in
    let masks = masks8 in
    pack_n l width masks
  else if num_size <= 16 then
    let width = 16 in
    let l = pack_longs_n width l in
    let masks = masks16 in
    pack_n l width masks
  else if num_size <= 32 then
    let width = 32 in
    let l = pack_longs_n width l in
    let masks = masks32 in
    pack_n l width masks
  else
    failwith "Pack support bit counds less than 32"

let unpack l num_size =
  let unpack_n width masks =
      let mask = masks (num_size - 1) in
      let shift_count = width / num_size in
      let shifts = List.init shift_count (fun i -> width - (i+1)*num_size) in
      let nums = List.concat_map (fun shift ->
        List.map (fun n ->
          Int64.logand (Int64.shift_right_logical n  shift) mask) l) shifts in
      let extra = width mod num_size in
      let packed_nums = if extra = 0 then
        nums
      else
        List.concat [nums; repack_bits l masks extra num_size ] in
      unpack_longs_n width packed_nums in
  if num_size <= 8 then
    let width = 8 in
    let masks = masks8 in
    unpack_n width masks
  else if num_size <= 16 then
    let width = 16 in
    let masks = masks16 in
    unpack_n width masks
  else
    failwith "Not implemented yet"


module Encode(Data_output: Data_output.S) = struct
  let encode l num_size out =
    let ol = pack l num_size in
    List.iter (Data_output.write_le_int64 out) ol
end


module Decode(Data_input: Data_input.S) = struct
  let decode num_size di =
    let len = 2*num_size in
    let l = List.init len (fun _ -> Data_input.read_le_int64 di) in
    unpack l num_size
end

(*(* Decode a list of bit packed numbers 8 bit numbers.*)
(*  The output will be unpacked into num_size ints.*)
(*  *)*)
(*let decode l num_size = repack_bits l 8 num_size*)



