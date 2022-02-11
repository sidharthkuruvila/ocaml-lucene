type t = string

let get_byte = String.get

let copy_bytes src dest ~src_index ~dest_index ~length =
   String.blit src src_index dest dest_index length

let length = String.length