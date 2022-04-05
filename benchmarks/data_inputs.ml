module type Mutable_data_input_ops = sig
  type t

  (**
   * Read a single byte and update the current position
   *)
  val read_byte : t -> int

end

module Make_mutable_data_input(M : Mutable_data_input_ops) = struct
  include M
  let read_int di =
    let b1 = read_byte di in
    let b2 = read_byte di in
    let b3 = read_byte di in
    let b4 = read_byte di in
    (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4

end

module String_mutable_data_input = Make_mutable_data_input(struct
 type t = {
   s: string;
   mutable n: int;
 }
 let read_byte di =
  let v = String.get di.s 0 in
  di.n <- di.n + 1;
  int_of_char v
end)

module type Immutable_data_input_ops = sig
  type t

  (**
   * Read a single byte and update the current position
   *)
  val read_byte : t -> t * int

end

module Make_immutable_data_input(M : Immutable_data_input_ops) = struct
  include M
  let read_int di =
    let (di, b1) = read_byte di in
    let (di, b2) = read_byte di in
    let (di, b3) = read_byte di in
    let (di, b4) = read_byte di in
    di, (b1 lsl 24) lor (b2 lsl 16) lor (b3 lsl 8) lor b4
end

module String_immutable_data_input = Make_immutable_data_input(struct
 type t = {
   s: string;
   n: int;
 }
 let read_byte di =
  let v = String.get di.s 0 in
  ({di with n = di.n + 1}, int_of_char v)
end)