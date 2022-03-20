
module type S = sig
  type 'a t
  type state
  module Output: Output.S
  val compile_state: (state, Output.t) State.t -> state t
  val (let*): 'a t -> ('a -> 'b t) -> 'b t
  val return: 'a -> 'a t
  val fold_left: ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t
  val fold_right: ('a -> 'b -> 'b t) -> 'a list -> 'b t -> 'b t
end