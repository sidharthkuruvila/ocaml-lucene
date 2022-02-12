type t = string [@@deriving show]

let common_prefix_length s1 s2 =
 let n = Int.min (String.length s1) (String.length s2) in
 let rec loop i =
   if i = n || not (Char.equal (String.get s1 i) (String.get s2 i)) then i
   else loop (i + 1) in
 loop 0

let longest_common_prefix s1 s2 =
   let l = common_prefix_length s1 s2 in
   String.sub s1 0 l


(* Return what is left from the second string
once the common prefix is removed *)
let remainder s1 s2 =
  let l = common_prefix_length s1 s2 in
  String.sub s2 l (String.length s2 - l)

let concat s1 s2 = s1 ^ s2

let compare = String.compare
let common = longest_common_prefix
let subtract = remainder
let add = concat
let to_string s = s
let empty = ""