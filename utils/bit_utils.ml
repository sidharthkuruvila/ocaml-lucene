let binary32 n =
  let open Int32 in
  let bytes = Bytes.make 32 '0' in
  let rec loop i =
    let c = if logand (shift_left 1l i) n = 0l then
      '0'
    else
      '1' in
    Bytes.set bytes (31 - i) c;
    if i < 31 then loop (i + 1) in
  loop 0;
  Bytes.to_string bytes

let binary64 n =
  let open Int64 in
  let bytes = Bytes.make 64 '0' in
  let rec loop i =
    let c = if logand (shift_left 1L i) n = 0L then
      '0'
    else
      '1' in
    Bytes.set bytes (63 - i) c;
    if i < 63 then loop (i + 1) in
  loop 0;
  Bytes.to_string bytes


(*
  Here is a nice write up on zig zag encoding
  https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba#file-zigzag-encoding-readme
*)

let zig_zag_encode_int n = Int.logxor (Int.shift_right n Sys.int_size) (n lsl 1)

let zig_zag_decode_int n = Int.logxor (n lsr 1) (- (n land 1))

(*
  Find the index of the most significant bit.
  The digits are 1 index the lsb being at index 1

  This function will only work with positive integers.

  For example
    msb 0 = 0
    msb 1 = 1
    msb 2 = 2
*)
let msb n =
  let rec loop i n c =
    let (n, c) = if n >= 1 lsl i then
      (n lsr i, c + i)
    else
      (n, c) in
    if i = 0 then
      c
    else
      loop (i / 2) n c in
  if n = 0 then
    0
  else
    loop 32 n 1

