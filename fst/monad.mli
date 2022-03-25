module type S = sig
  type 'a t

  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val map: 'a t -> ('a -> 'b) -> 'b t
  val return: 'a -> 'a t
end

module Ops: sig
  module Make (M: S): sig
    module Infix: sig
      val (let*): 'a M.t -> ('a -> 'b M.t) -> 'b M.t
      val (>>|): 'a M.t -> ('a -> 'b) -> 'b M.t
      val (>>=): 'a M.t -> ('a -> 'b M.t) -> 'b M.t
    end
    val fold_left: ('a -> 'b -> 'a M.t) -> 'a M.t -> 'b list -> 'a M.t
    val fold_right: ('a -> 'b -> 'b M.t) -> 'a list -> 'b M.t -> 'b M.t
  end
end

module Identity: sig
  type 'a t = 'a
  val bind: 'a -> ('a -> 'b) -> 'b
  val map: 'a -> ('a -> 'b) -> 'b
  val return: 'a -> 'a
end