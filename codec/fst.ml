open Lucene_data_input
open Lucene_utils

let arcs_for_direct_addressing = 1 lsl 6
let bit_last_arc = 1 lsl 1
let bit_final_arc = 1 lsl 0


let bit_target_next = 1 lsl 2

let bit_arc_has_output = 1 lsl 4
let bit_arc_has_final_output = 1 lsl 5
let check_flag byte flag = byte land flag != 0

let  bit_stop_node = 1 lsl 3

let no_output = ""
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
  start_node: int;
  num_bytes: int;
  index_in: Index_input.t;
  index_file_pointer: int;
}

module Arc =  struct
 type t = {
   target: int;
   output: string;
   final_output: string;
   next_arc: int option;
 }

 let show arc =
   let { target; next_arc; output; final_output } = arc in
   let next_arc_s = match next_arc with | Some n -> string_of_int n | None -> "<empty>" in
   Printf.sprintf "Arc { node: %d, output: '%s', final_output: '%s', next_arc: %s }"
     target (Hex_util.hex_of_string output) (Hex_util.hex_of_string final_output) next_arc_s
end

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
  let start_node = Int64.to_int (Index_input.read_vlong meta_in) in
  let num_bytes = Int64.to_int (Index_input.read_vlong meta_in) in
  let index_file_pointer = Index_input.get_file_pointer index_in in
  {
    empty_output;
    input_type;
    start_node;
    num_bytes;
    index_in;
    index_file_pointer;
  }

let get_reverse_reader fst =
  Printf.printf "File pointer: %d, num bytes: %d\n" fst.index_file_pointer fst.num_bytes;
  let sub = Index_input.slice fst.index_in fst.index_file_pointer fst.num_bytes in
  print_endline "Created sub array";
  Reversed_index_input.from_index_input sub

let is_bit_set arc_index ~input ~bit_table_start =
  Reversed_index_input.set_position input bit_table_start;
  let get_byte_at n =
    Reversed_index_input.get_byte input n in
  Bit_set_util.is_bit_set ~get_byte_at arc_index

let count_bits_upto arc_index ~input ~bit_table_start =
  Reversed_index_input.set_position input bit_table_start;
  let get_byte_at n =
    Reversed_index_input.get_byte input n in
  Bit_set_util.count_bits_upto ~get_byte_at arc_index

let count_bits byte_count ~input ~bit_table_start =
  Reversed_index_input.set_position input bit_table_start;
  let get_byte_at n =
    Reversed_index_input.get_byte input n in
  Bit_set_util.count_bits ~get_byte_at byte_count

let next_arc_using_direct_addressing label ~input =
  (* The arcs are layed out for direct addressing. This
     is a two step process.

     First look up a bit table to identify if the outgoing arc
     exists. Then count the set bits up to the arcs position to get
     the arc's index in the arc list.

     num_arcs the number of arcs represented in the bit table
     bytes_per_arc the number of bytes allocated to each arc in the
        arc list.
     bit_table_start the pointer to the start of the bit table
       the size of the bit table is based on the number of arcs
     first_label the first label in the arc list. Both the bit table
       and the arc index are indexed starting with the first_label as 0.
     arc_index the index of the label to find the next arc.
     presence_index the index of the arc in the arc list

     *)
  let num_arcs = Reversed_index_input.read_vint input in
  let bytes_per_arc = Reversed_index_input.read_vint input in
  let bit_table_start = Reversed_index_input.get_position input in
  let presence_byte_count = (num_arcs + 7) lsr 3 in
  Reversed_index_input.skip_bytes input presence_byte_count;
  let first_label = Reversed_index_input.read_byte input in
  let pos_arc_start = Reversed_index_input.get_position input in
  let arc_index = label - first_label in
  if arc_index < 0 || arc_index >= num_arcs then
    None
  else if not (is_bit_set arc_index ~input ~bit_table_start) then
    None
  else
    let presence_index = count_bits_upto arc_index ~input ~bit_table_start in
    Printf.printf "Presence Index = %d, arc index = %d, bytes_per_arc = %d\n" presence_index arc_index bytes_per_arc;
    Reversed_index_input.set_position input (pos_arc_start - presence_index * bytes_per_arc);
    let flags = Reversed_index_input.read_byte input in
    Printf.printf "flags = %s\n" (Bit_utils.binary32 (Int32.of_int flags));
    let output = if check_flag flags bit_arc_has_output then
        Reversed_index_input.read_string input
      else
        no_output in
    let final_output = if check_flag flags bit_arc_has_final_output then
        Reversed_index_input.read_string input
      else
        no_output in
    let is_stop_node = check_flag flags bit_stop_node in
    let is_final_arc = check_flag flags bit_final_arc in
    if is_stop_node then begin
      Some {
         Arc.target = if is_final_arc then -1 else 0;
         output;
         final_output;
         next_arc = Some (Reversed_index_input.get_position input)
      }
    end else if check_flag flags bit_target_next then
      let target, next_arc = if check_flag flags bit_last_arc then
          (Reversed_index_input.get_position input, None)
        else
          let arc_count = count_bits presence_byte_count ~input ~bit_table_start in
          (pos_arc_start - bytes_per_arc * arc_count, Some (Reversed_index_input.get_position input)) in
      Some {
        Arc.target;
        output;
        final_output;
        next_arc;
      }
    else
      let target = Int64.to_int (Reversed_index_input.read_vlong input) in
      let next_arc = Some (Reversed_index_input.get_position input) in
      Some {
        Arc.target;
        output;
        final_output;
        next_arc;
      }

let next_arc label ~input ~arc =
  (* If the target is either 1 or 0
     there are no following arcs. *)
  if arc.Arc.target <= 0 then
    None
  else begin
    (* The target is a pointer to the next node / state.
       Point the data input to the start of the node. *)
    Reversed_index_input.set_position input arc.Arc.target;
    (* Get the lookup strategy for outgoing arcs on the node.
       The strategy can be.

       * Direct Addressing
       * Binary Search
       * Scan *)
    print_endline "about to read flags";
    let flags = Reversed_index_input.read_byte input in
    print_endline "done reading flags";
    if flags = arcs_for_direct_addressing then
      next_arc_using_direct_addressing label ~input
    else
      failwith "not implemented yet, only next_arc_using_direct_addressing has been implemented"
  end

let fst_match_term ~fst term =
  let input = get_reverse_reader fst in
  let start_arc = {
    Arc.target = fst.start_node;
    output = "";
    final_output = "";
    next_arc = None;
  } in
  let target_length = String.length term in
  let rec loop prev_arc n =
    if n = target_length then
      [prev_arc]
    else
      let label = int_of_char (String.get term n) in
      Printf.printf "loop index %d\n" n;
      let arc_option = next_arc label ~input ~arc:prev_arc in
      match arc_option with
      | Some arc -> print_endline (Arc.show arc); prev_arc::(loop arc (n + 1))
      | None -> [prev_arc] in
  let path = loop start_arc 0 in
  path