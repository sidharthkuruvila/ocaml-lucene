module Wrapper = struct
  type t = Buffer.t

  let write_byte out b =
    Buffer.add_char out b

end

include Data_output.Make(Wrapper)

let from_buffer b = b