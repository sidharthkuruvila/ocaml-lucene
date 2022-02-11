type t = (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

let from_fd fd =
  let st = Unix.fstat fd in
  let sz = st.Unix.st_size in
  let map = Unix.map_file fd Bigarray.Int8_unsigned Bigarray.c_layout false [|sz|] in
  Bigarray.array1_of_genarray map

let get_byte src index = char_of_int (Bigarray.Array1.get src index)

let copy_bytes src dest ~src_index ~dest_index ~length =
   for i = 0 to length - 1 do
     let b = get_byte src (src_index + i) in
     Bytes.set dest (dest_index + i) b
   done

let length src = Bigarray.Array1.dim src