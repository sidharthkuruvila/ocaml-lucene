module Input_type = struct
  type t =
  | Byte1
  | Byte2
  | Byte4

  let from_code c =
  match c with
  | 0 -> Byte1
  | 1 -> Byte2
  | 2 -> Byte4
  | _ -> failwith "Input type did not match"
end


type t = {
  empty_output: string option;
  input_type: Input_type.t;
  start_node: Int64.t;
  num_bytes: Int64.t;
  index_in: Index_input.t;
  index_file_pointer: int;
}

module Arc = struct
  let no_output = ""
  type t = {
(*    output: string option;*)
    target: int;
(*    next_final_output: string;*)
(*    next_arc: Int64.t;*)
(*    is_final_arc: bool;*)
(*    is_last_arc: bool;*)
(*    target_next: bool;*)
  }

end


let first_arc fst =
    {
      Arc.target = Int64.to_int fst.start_node;
(*      Arc.output = Some "";*)
(*      is_final_arc = true;*)
(*      is_last_arc = true;*)
(*      next_final_output = fst.empty_output;*)

    }
(*
let find_target_arc fst label follow =
  let in = fst.index_in in
  in.setPosition(follow.target());
*)


let read ~meta_in ~index_in =
  Codec_util.check_header ~codec_name:"FST" ~min_version:6 ~max_version:7 meta_in;
  let empty_output = if Index_input.read_byte meta_in = 1 then
    let num_bytes = Index_input.read_vint meta_in in
    let arr = Index_input.read_bytes meta_in num_bytes in
    Some arr
  else
    None in
  let t = Index_input.read_byte meta_in in
  let input_type = Input_type.from_code t in
  let start_node = Index_input.read_vlong meta_in in
  let num_bytes = Index_input.read_vlong meta_in in
  let index_file_pointer = Index_input.get_file_pointer index_in in
  {
    empty_output;
    input_type;
    start_node;
    num_bytes;
    index_in;
    index_file_pointer;
  }

