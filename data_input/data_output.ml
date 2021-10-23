module type S = sig
  type t
  val write_byte: t -> char -> unit
  val write_vint: t -> int -> unit
  val write_le_int64: t -> Int64.t -> unit
end


module type Data_output_ops = sig
  type t
  val write_byte: t -> char -> unit
(*  val write_bytes: t -> start:int -> length:int -> String.t*)
end

module Make(M: Data_output_ops) = struct
  include M
  let write_vint out n =
    let rec loop n =
    if n < 128 then
      write_byte out (char_of_int n)
    else begin
      write_byte out (char_of_int ((n land 127) + 128));
      loop (n lsr 7)
    end in
  loop n

  let write_le_int64 out n =
    let rec loop n count =
      if count != 0 then begin
        write_byte out (char_of_int (Int64.to_int (Int64.logand n  255L)));
        loop (Int64.shift_right_logical n  8) (count - 1)
      end in
    loop n 8
end