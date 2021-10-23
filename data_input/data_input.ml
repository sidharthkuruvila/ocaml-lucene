module type S = sig
  type t

  val read_byte : t -> int
  val read_bytes : t -> int -> string
  val get_byte : t -> int -> int
  val set_position : t -> int -> unit
  val copy: t -> t

  val read_int : t -> int
  val read_long : t -> Int64.t
  val read_vlong : t -> Int64.t
  val read_vint : t -> int
  val read_uint: t -> int
  val read_string: t -> String.t
  val read_list_of_strings: t -> string list
  val read_assoc_list_of_strings: t -> (string * string) list
  val read_le_int64 : t -> Int64.t
end


module type Data_input_ops = sig
  type t

  (**
   * Read a single byte and update the current position
   *)
  val read_byte : t -> int

  (**
   * Read n bytes and update the current position
   *)
  val read_bytes : t -> int -> string

  (**
   * Get a single byte at n bytes from current position
   *)
  val get_byte : t -> int -> int

  (**
   * Set the current position
   *)
  val set_position : t -> int -> unit

  (**
   * Get the current position
   *)
  val get_position : t -> int

  (**
   * Create a clone of the data input that will
   * update its position independently
   *)
  val copy: t -> t
end


module Make(M : Data_input_ops) = struct
  include M
  let read_int di =
    let b1 = read_byte di in
    let b2 = read_byte di in
    let b3 = read_byte di in
    let b4 = read_byte di in
    (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

  let read_long di =
    let open Int64 in
    let rec loop n c =
      if c = 0 then
        n
      else
      let b = read_byte di in
      let m = logor (shift_left n 8) (of_int b) in
      loop m (c - 1) in
    loop (Int64.of_int 0) 8

  (*
    Avoid bounds checking for the read vlong as we can expect
    the final byte to always be less than or equal to 127
  *)
  let read_vlong di =
    let open Int64 in
    let rec loop acc shift =
      let b = of_int (read_byte di) in
      let d = logand b (of_int 127) in
      let n = logor acc (shift_left d shift) in
      if d = b then
        n
      else loop n (shift + 7) in
    loop (of_int 0) 0

  let read_vint di =
    let rec loop acc shift =
      let b = read_byte di in
      let d = b land 127 in
      let n = acc lor (d lsl shift) in
      if d = b then
        n
      else loop n (shift + 7) in
    loop 0 0

  let read_le_int64 di =
    let rec loop n count =
      if count != 0 then
        let b = read_byte di in
        loop (Int64.logor (Int64.shift_left n  8) (Int64.of_int b)) (count - 1)
      else
        n in
    loop 0L 8

  let read_uint di =
    read_int di

  let read_string di =
    let sz = read_vint di in
    read_bytes di sz

  let read_list_of_strings di =
    let count = read_vint di in
    let rec loop n =
      if n = 0 then
        []
      else
        read_string di :: loop (n - 1) in
    loop count

  let read_assoc_list_of_strings di =
    let count = read_vint di in
    let rec loop n =
      if n = 0 then
        []
      else
        let k = read_string di in
        let v = read_string di in
        (k, v) :: loop (n - 1) in
    loop count



end

