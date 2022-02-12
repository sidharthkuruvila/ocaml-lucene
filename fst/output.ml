
module type S = sig
  type t
  val compare : t -> t -> int
  val common : t -> t -> t
  val subtract : t -> t -> t
  val add : t -> t -> t
  val to_string : t -> string
  val empty: t
  val pp: Format.formatter -> t -> unit
end
