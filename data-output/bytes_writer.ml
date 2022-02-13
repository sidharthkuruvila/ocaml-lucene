module type S = sig
  type t = Buffer.t
  val write_byte: t -> char -> unit
end