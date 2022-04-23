module Make(Src_bytes: Byte_array.S): sig
   include Byte_array.S
   val slice: Src_bytes.t -> int -> int -> t
end
