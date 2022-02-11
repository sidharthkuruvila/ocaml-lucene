module type S = sig
  type t

  val read_byte : t -> int
  val read_bytes : t -> int -> string
  val get_byte : t -> int -> int
  val skip_bytes : t -> int -> unit

  val get_position : t -> int
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

module Make(M : Bytes_source.S) = struct
  open M
  let read_byte_as_int di = int_of_char (read_byte di)


  let read_int di =
    let open Int32 in
    let read_byte di =
      read_byte di |> int_of_char |> of_int in
    let b1 = read_byte di in
    let b2 = read_byte di in
    let b3 = read_byte di in
    let b4 = read_byte di in
    let n = logor (shift_left b4 24)  (logor (shift_left b3 16) (logor (shift_left b2  8) b1)) in
    Int32.to_int n

  (* This will truncate the 64 bit long to fit into the space avilable to an int *)
  let read_long di =
    let open Int64 in
    let read_byte di =
      read_byte di |> int_of_char |> of_int in
    let rec loop n c =
      if c = 8 then
        n
      else
      let b = read_byte di in
      let m = logor n (shift_left b (c*8)) in
      loop m (c + 1) in
    let n = loop (Int64.of_int 0) 0 in
    Int64.to_int n

  (*
    Avoid bounds checking for the read vlong as we can expect
    the final byte to always be less than or equal to 127
  *)
  let read_vlong di =
    let open Int64 in
    let rec loop acc shift =
      let b = of_int (read_byte_as_int di) in
      let d = logand b (of_int 127) in
      let n = logor acc (shift_left d shift) in
      if d = b then
        n
      else loop n (shift + 7) in
    let n = loop (of_int 0) 0 in
    to_int n

  let read_vint di =
    let open Int32 in
    let read_byte di =
      read_byte di |> int_of_char |> of_int in
    let rec loop acc shift =
      let b = read_byte di in
      let d = logand b (of_int 127) in
      let n = logor acc (shift_left d shift) in
      if d = b then
        n
      else loop n (shift + 7) in
    let n = loop (of_int 0) 0 in
    to_int n

  let read_le_int64 di =
    let rec loop n count =
      if count != 8 then
        let b = read_byte_as_int di in
        loop (Int64.logor n (Int64.shift_left (Int64.of_int b) (count  * 8))) (count + 1)
      else
        n in
    loop 0L 0

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
        Printf.printf "Key = %s\n" k;
        let v = read_string di in
        Printf.printf "Value = %s\n" v;
        (k, v) :: loop (n - 1) in
    loop count

end