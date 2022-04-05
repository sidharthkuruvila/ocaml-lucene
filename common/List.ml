open List

let rec take_n n l  =
  if n = 0 then
    []
  else
    match l with
    | x::xs -> x::(take_n (n - 1) xs)
    | [] -> []

let rec drop_n n l =
 if n = 0 then
   l
 else
 match l with
 | _::xs -> drop_n (n - 1) xs
 | [] -> []
