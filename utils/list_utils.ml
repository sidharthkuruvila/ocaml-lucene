let rec fill n v =
  if n = 0 then []
  else v::(fill (n - 1) v)

let rec last l =
  match l with
  | [] -> failwith "Last expects at least one item in the list"
  | [x] -> x
  | _::rest -> last rest

let grouped group_size l  =
  let rec loop l fill lol n =
    match l with
    | [] -> List.rev ((List.rev fill) :: lol)
    | _ when n = 0 ->
        loop l [] ((List.rev fill)::lol) group_size
    | x::rest ->
        loop rest (x::fill) lol (n - 1) in
  loop l [] [] group_size

let zip_l l =
  let rec firsts l fs rs=
    match l with
    | [] -> Some (List.rev fs, List.rev rs)
    | (f::r)::rest -> firsts rest (f::fs) (r::rs)
    | _ -> None in
  let rec loop l =
    match firsts l [] [] with
    | None -> []
    | Some(row, rest) -> row::(loop rest) in
  loop l

let rec take_n l n =
  if n = 0 then
    ([], l)
  else
    match l with
    | x::rest ->
      let (l1, l2) = take_n rest (n - 1) in
      (x::l1, l2)
    | _ -> failwith "taken expects at least n items"