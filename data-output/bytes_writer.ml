module type S = sig
  type t = Buffer.t
  val write_byte: t -> char -> unit
  val length: t -> int
end