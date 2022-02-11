module type S = sig
 type t
 val close: t -> unit
 val get_byte: t -> Int.t -> Char.t
 val copy_bytes: t -> Bytes.t -> src_index:Int.t -> src_length:Int.t -> dest_index:Int.t -> dest_length:Int.t -> unit
 val length: t -> Int.t
end