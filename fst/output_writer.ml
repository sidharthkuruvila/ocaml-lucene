module type S = sig
  type t
  type data_output

  val write: data_output -> t -> unit
end