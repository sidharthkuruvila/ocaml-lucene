type t = string

let get_segment_file dir =
  let files = Array.to_list(Sys.readdir dir) in
  List.find (fun s -> String.length s >= 8 && String.sub s 0 8 = "segments") files

let open_input dir name =
  let fn = Printf.sprintf "%s/%s" dir name in
  let f = Unix.openfile fn [Unix.O_RDONLY] 0 in
  Index_input.from_fd f


let open_input_with ~f dir name =
   let fn = Printf.sprintf "%s/%s" dir name in
   let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
   let di = Index_input.from_fd fd in
   let close _ = Index_input.close di in
   try
     let res = f di in
     close ();
     res
   with exn -> close (); raise exn

