
let char_map = "0123456789ABCDEF"

let hex_of_string s =
  let len = String.length s in
  let buf = Buffer.create (len * 2) in
  for i = 0 to len - 1 do
    let c = String.get s i in
    let c1 = (int_of_char c) lsr 4 in
    let c2 = 15 land (int_of_char c) in
    Buffer.add_char buf (String.get char_map c1);
    Buffer.add_char buf (String.get char_map c2);
    Buffer.add_char buf ' '

  done;
  Bytes.to_string (Buffer.to_bytes buf)