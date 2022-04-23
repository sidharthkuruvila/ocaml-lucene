
module Make(Src_bytes: Byte_array.S) = struct
  type t = {
    src: Src_bytes.t;
    start: int;
    length: int;
  }

  let slice src start length =
    { src; start; length }

  let get_byte { src; start; _ } n =
    Src_bytes.get_byte src (start + n)

  let copy_bytes { src; start; _ } dest ~src_index ~dest_index ~length =
     Src_bytes.copy_bytes src dest ~src_index:(start + src_index)  ~dest_index ~length

  let length { length; _ } = length
end