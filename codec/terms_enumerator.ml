open Lucene_data_input

let arcs_for_direct_addressing = 1 lsl 6
let bit_last_arc = 1 lsl 1
let bit_final_arc = 1 lsl 0


let bit_target_next = 1 lsl 2

let bit_arc_has_output = 1 lsl 4
let bit_arc_has_final_output = 1 lsl 5
let check_flag byte flag = byte land flag != 0

let output_flags_num_bits = 2

let  bit_stop_node = 1 lsl 3


let no_output = ""

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
    Printf.printf "flags = %s\n" (Bit_set_util.binary32 (Int32.of_int flags));
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

let find_block output target_label =
  (* Get the file pointer for the block likely to contain the term
     the target_label is the chracter after the prefix. Each block contains terms
     for a range of target_label characters. For example if the target_label is g and
     the first block contains terms that would have matched for target_labels up to e
     we need to move to the next block. *)
  let output_reader = String_data_input.from_string output in
  let code = String_data_input.read_vlong output_reader in
  let fp = Int64.to_int (Int64.shift_right_logical code output_flags_num_bits) in
  let has_terms = Int64.logand code 2L <> 0L in
  let is_floor = Int64.logand code 1L <> 0L in
  Printf.printf "fp = %d, has_terms = %b, is_Floor = %b\n" fp has_terms is_floor;
  if not is_floor then failwith "Block is not floor, don't know how to handle that";
  let num_floor_follow_blocks = String_data_input.read_vint output_reader in
  let next_floor_label = String_data_input.read_byte output_reader in
  Printf.printf "Num floor follow blocks: %d, next floor label %d\n" num_floor_follow_blocks next_floor_label;
  if target_label < next_floor_label then
    (fp, has_terms)
  else
    let rec loop n fp =
      if n = 0 then failwith "Should never run out of blocks to scan";
      let code = String_data_input.read_vlong output_reader in
      let new_fp = fp + Int64.to_int (Int64.shift_right_logical code 1) in
      let has_terms = Int64.logand code 1L <> 0L in
      if num_floor_follow_blocks = 1 then
        (new_fp, has_terms)
      else
        let next_floor_label = String_data_input.read_byte output_reader in
        if target_label < next_floor_label then
          (new_fp, has_terms)
        else
          loop (n - 1) new_fp in
    loop num_floor_follow_blocks fp

let seek_exact ~field_reader ~fst target =
  print_endline "seeking exact";
  Printf.printf "target = %s size = %d\n" target (Field_reader.get_size field_reader);
  print_endline field_reader.min_term ;
  print_endline field_reader.max_term;
  if Field_reader.get_size field_reader > 0 && (String.compare target field_reader.min_term < 0 || String.compare target field_reader.max_term > 0) then
    None
  else
    let input = Fst.get_reverse_reader fst in
    let start_arc = {
      Arc.target = fst.Fst.start_node;
      output = "";
      final_output = "";
      next_arc = None;
    } in
    let target_length = String.length target in
    let rec loop prev_arc n =
      if n = target_length then
        [prev_arc]
      else
        let label = int_of_char (String.get target n) in
        Printf.printf "loop index %d\n" n;
        let arc_option = next_arc label ~input ~arc:prev_arc in
        match arc_option with
        | Some arc -> print_endline (Arc.show arc); prev_arc::(loop arc (n + 1))
        | None -> [prev_arc] in
    let path = loop start_arc 0 in
    let build_output path =
      let buf = Buffer.create 10 in
      let rec loop path =
        match path with
        | [] -> failwith "Empty path"
        | [last] ->
          Buffer.add_string buf last.Arc.output;
          Buffer.add_string buf last.Arc.final_output
        | x::rest ->
          Buffer.add_string buf x.Arc.output;
          loop rest in
      loop path;
      Bytes.to_string (Buffer.to_bytes buf) in
    let output = build_output path in
    Printf.printf "Output = %s\n" (Hex_util.hex_of_string output);
    (* SegmentTermsEnum::pushFrame *)
    (* SegmentTermsEnumFrame::scanToFloorFrame *)
    (* The length of the prefix is the length of path traversed along the arcs minus 1 *)
    let prefix_length = List.length path - 1 in
    let target_label = int_of_char (String.get target prefix_length) in
    let (fp, has_terms) = find_block output target_label in
    Printf.printf "Prefix length = %d, fp = %d, has_terms = %b \n" prefix_length fp has_terms;
    List.iteri (fun i arc -> Printf.printf "%d -> %s\n" i (Arc.show arc)) path;

(*    let arc = next_arc label ~input ~arc:start_arc |> Option.get in*)
    (*Reversed_index_input.set_position input fst.Fst.start_node;
    let flags = Reversed_index_input.read_byte input in*)
(*    Printf.printf "Arc = %s\n" (Arc.show arc);*)
    None
    (*let input = Fst.get_reverse_reader fst in
    let rec loop n =
      if n = Field_reader.get_size field_reader then
        Some true
      else
        let label = String.get target n in
        Reversed_index_input.set_position input fst.Fst.start_node;
        let flags = Reversed_index_input.read_byte input in
        if flags = arcs_for_direct_addressing then
          let num_arcs = Reversed_index_input.read_vint input in
          let bytes_per_arc = Reversed_index_input.read_vint input in
          let bit_table_start = Reversed_index_input.get_position input in
          Reversed_index_input.skip_bytes input ((num_arcs + 7) lsl 3);
          let first_label = Reversed_index_input.read_byte input in
          let pos_arc_start = Reversed_index_input.get_position input in
          let arc_index = label - first_label in
          if arc_index < 0 || arc_index >= num_arcs then
            None
          else if not (is_bit_set arc_index ~input ~bit_table_start) then
            None
          else
            let presence_index = count_bits_upto arc_index ~input ~bit_table_start in
            Reversed_index_input.set_position input (pos_arc_start - presence_index * bytes_per_arc);
            None
        else
          None in
    loop 0*)


