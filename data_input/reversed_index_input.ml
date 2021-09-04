module Wrapper = struct
  type t = {
    data: (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
    mutable pos: int;
  }


  let read_byte rii =
    let res = Bigarray.Array1.get rii.data rii.pos in
    rii.pos <- rii.pos - 1;
    res

  let read_bytes rii n =
    let bs = Bytes.create n in
    for i = 0 to n - 1 do
      let b = Bigarray.Array1.get rii.data (rii.pos - i) in
      Bytes.set bs i (char_of_int b)
    done;
    rii.pos <- rii.pos - n;
    Bytes.to_string bs

  let get_byte rii n = Bigarray.Array1.get rii.data (rii.pos - n)

  let set_position rii n = rii.pos <- n

  let get_position rii = rii.pos

  let copy rii = { rii with pos = rii.pos }
end

include Data_input.Make(Wrapper)

let from_index_input input = {
  Wrapper.data = Index_input.get_backing_array input;
  pos = 0
}