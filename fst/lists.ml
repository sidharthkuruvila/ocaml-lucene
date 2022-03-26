let rec split_at_index ~index l =
  match index, l with
  | 0, l -> ([], l)
  | n, x::rest ->
      let (l1, l2) = split_at_index ~index:(n - 1) rest in
      (x::l1, l2)
  | _, [] -> ([], [])

let rec drop_n ~n l =
  if n = 0 then
    l
  else
  match l with
  | _::xs -> drop_n ~n:(n - 1) xs
  | [] -> []

let common_prefix_length list1 list2 ~compare =
  let rec loop i w1 w2 =
    match (w1, w2) with
    | (a::r1, b::r2) when compare a b = 0 -> loop (i + 1) r1 r2
    | _ -> i in
  loop 0 list1 list2

let is_empty list =
  match list with
  | [] -> true
  | _ -> false