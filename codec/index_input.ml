(*
 * The wrapper module exists so that we can have a module to construct
 * We then include all the functions in the wrapper to the top level for
 * ease of use.
 *)
module Wrapper = struct
type t = {
  fd: Unix.file_descr;
  data: (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  mutable idx: int;
}



let copy di = {di with idx = di.idx}

let get_byte di i =
 Bigarray.Array1.get di.data  (di.idx + i)

let inc_idx di n =
  di.idx <- di.idx + n

let read_byte di =
  let b = get_byte di 0 in
  inc_idx di 1;
  b

let read_bytes di sz =
  let bytes = Bytes.create sz in
  for i = 0 to sz - 1 do
    let b = read_byte di in
    Bytes.set bytes i (char_of_int b)
  done;
  Bytes.to_string bytes
end
include Wrapper
include Data_input.Make(Wrapper)

let from_fd fd =
  let st = Unix.fstat fd in
  let sz = st.Unix.st_size in
  let map = Unix.map_file fd Bigarray.Int8_unsigned Bigarray.c_layout false [|sz|] in
  {
    fd = fd;
    data = Bigarray.array1_of_genarray map;
    idx = 0;
  }

let close di =
  Unix.close di.fd

let get_file_pointer di = di.idx

let set_file_pointer di idx = di.idx <- idx

let get_file_length di =
  let st = Unix.fstat di.fd in
  st.Unix.st_size