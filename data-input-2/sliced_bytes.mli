module Make(Src_bytes: Bytes_intf.S): sig
   include Bytes_intf.S
   val slice: Src_bytes.t -> int -> int -> t
end
