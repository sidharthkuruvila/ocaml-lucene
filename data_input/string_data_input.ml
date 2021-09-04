module Wrapper = struct
  type t = {
    data: string;
    mutable pos: int;
  }


  let read_byte rii =
    let res = String.get rii.data rii.pos in
    rii.pos <- rii.pos + 1;
    (int_of_char res)

  let read_bytes rii n =
    let bs = Bytes.create n in
    for i = 0 to n - 1 do
      let b = String.get rii.data (rii.pos + i) in
      Bytes.set bs i b
    done;
    rii.pos <- rii.pos + n;
    Bytes.to_string bs

  let get_byte rii n = int_of_char (String.get rii.data (rii.pos + n))

  let set_position rii n = rii.pos <- n

  let get_position rii = rii.pos

  let copy rii = { rii with pos = rii.pos }
end

include Data_input.Make(Wrapper)

let from_string str = {
  Wrapper.data = str;
  pos = 0
}
