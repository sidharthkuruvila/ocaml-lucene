let rev_bytes bytes =
  let n = Bytes.length bytes - 1 in
  for i = 0 to (Bytes.length bytes - 1) / 2 do
    let t = Bytes.get bytes i in
    let u = Bytes.get bytes (n - i) in
   Bytes.set bytes (n - i) t;
   Bytes.set bytes i u
  done

module Make(M: Bytes_intf.S) = struct
  type t = {
    data: M.t;
    mutable idx: Int.t
  }

  let of_bytes data = {
    data;
    idx = (M.length data - 1);
  }

  let copy di = {di with idx = di.idx}

  let get_byte di i =
    M.get_byte di.data (di.idx - i)

  let decr_idx di n =
    di.idx <- di.idx - n

  let read_byte di =
    let b = get_byte di 0 in
    decr_idx di 1;
    b

  let read_bytes di sz =
    let bytes = Bytes.create sz in
    M.copy_bytes di.data bytes ~src_index:(di.idx - sz + 1) ~dest_index:0 ~length:sz;
    decr_idx di sz;
    rev_bytes bytes;
    Bytes.to_string bytes

  let get_position di = di.idx

  let set_position di idx = di.idx <- idx

  let skip_bytes = decr_idx

  let length { data; _ } = M.length data
end