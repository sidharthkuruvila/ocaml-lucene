module type S = sig
  (** The monad type used to stitch compile computations. *)
  type 'a t

  (** The compiled state. *)
  type state

  (** The output used by the module. *)
  module Output: Output.S

  (** [compile_state uncompiled_state] Compile an uncompiled state. *)
  val compile_state: (state, Output.t) State.t -> state t

  (** [let* a = m in a] Get the value in the monad.

  This is the equavalent of the bind operation. *)
  val (let*): 'a t -> ('a -> 'b t) -> 'b t

  (** [return a] Wrap a in the transducer monad. *)
  val return: 'a -> 'a t

  (** [fold_left f m l] Apply f on the list l using a fold left. *)
  val fold_left: ('a -> 'b -> 'a t) -> 'a t -> 'b list -> 'a t

  (** [fold_right f l m] Apply f on the list l using a fold right. *)
  val fold_right: ('a -> 'b -> 'b t) -> 'a list -> 'b t -> 'b t

end