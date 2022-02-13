type t = int [@@deriving show]

let compare = Int.compare
let common = Int.min
let subtract = (-)
let add = (+)
let to_string = Int.to_string
let empty = 0
let is_empty n = n = 0