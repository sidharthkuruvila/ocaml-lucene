module type Context = sig
  val name: string
end

module Make(M : Context) = struct
  let print_message level msg = Printf.printf "%s(%s): %s\n" level M.name msg

  let info msg = print_message "INFO" msg

  let debug msg_fn = print_message "DEBUG" (msg_fn ())

  let error msg = print_message "ERROR" msg
end