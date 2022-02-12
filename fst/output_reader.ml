module type S = sig
  type t
  type data_input

  val read: data_input -> t
end