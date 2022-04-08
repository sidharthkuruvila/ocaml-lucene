module type S = sig
  type t

  val read_byte : t -> char
  val read_bytes : t -> int -> string
  val get_byte : t -> int -> char
  val skip_bytes : t -> int -> unit

  val get_position : t -> int
  val set_position : t -> int -> unit
  val length : t -> int
  val copy: t -> t
end

module Make(M: Bytes_intf.S) = struct
  type t = {
    data: M.t;
    mutable idx: Int.t
  }

  let of_bytes data = {
    data;
    idx = 0;
  }

  let copy di = {di with idx = di.idx}

  let get_byte di i =
    M.get_byte di.data (di.idx + i)

  let inc_idx di n =
    di.idx <- di.idx + n

  let read_byte di =
    let b = get_byte di 0 in
    inc_idx di 1;
    b

  let read_bytes di sz =
    let bytes = Bytes.create sz in
    M.copy_bytes di.data bytes ~src_index:di.idx ~dest_index:0 ~length:sz;
    inc_idx di sz;
    Bytes.to_string bytes

  let get_position di = di.idx

  let set_position di idx = di.idx <- idx

  let length di =
    M.length di.data

  let skip_bytes = inc_idx
end