module type S = sig
  type 'a t
  val bind: 'a t -> ('a -> 'b t) -> 'b t
  val map: 'a t -> ('a -> 'b) -> 'b t
  val return: 'a -> 'a t
end

module Ops = struct
  module Make(Monad: S) = struct
    open Monad
    module Infix = struct
      let (let*) = bind
      let (>>|) = map
      let (>>=) = bind
    end
    let fold_left f init l =
      bind init (fun init ->
      let rec loop l acc =
        match l with
        | [] -> return acc
        | x::rest ->
          bind (f acc x) (fun res -> loop rest res) in
      loop l init)

    let fold_right f l init =
      bind init (fun init ->
        let rec loop l =
          match l with
          | [] -> return init
          | x::rest ->
            bind (loop rest) (f x) in
        loop l
      )
  end
end

module Identity = struct
  type 'a t = 'a
  let bind m f = f m
  let map m f = f m
  let return m = m
end