
let check b msg =
  if b then failwith (Printf.sprintf "check failed: %s" msg)

let check_implemented b feature =
  if not b then failwith (Printf.sprintf "Feature %s has not been implemented yet" feature)