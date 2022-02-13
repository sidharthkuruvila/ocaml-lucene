module type S = sig
  type t
  val write_byte: t -> char -> unit
  val length: t -> int
  val write_bytes: t -> string -> unit
  val write_vint: t -> int -> unit
  val write_string: t -> string -> unit
end

module Make(M: Bytes_writer.S) = struct
  include M

  let write_vint out n =
    let open Int32 in
    let n = of_int n in
    let char_of_int32 n = (char_of_int (to_int n)) in
    let rec loop n =
      if (logand n 127l) = n then
        write_byte out (char_of_int32 n)
      else begin
        write_byte out (char_of_int32 (add (logand n 127l) 128l));
        loop (shift_right_logical n  7)
      end in
  loop n

  let write_bytes out s =
    String.iter (fun b -> write_byte out b) s

  let write_string out s =
    let length = String.length s in
    write_vint out length;
    write_bytes out s


end