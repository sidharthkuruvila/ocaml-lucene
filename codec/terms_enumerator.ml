open Lucene_data_input
open Lucene_utils

module Log = Logger.Make(struct
 let name = "terms_enumerator"
end)

let arcs_for_direct_addressing = 1 lsl 6
let bit_last_arc = 1 lsl 1
let bit_final_arc = 1 lsl 0


let bit_target_next = 1 lsl 2

let bit_arc_has_output = 1 lsl 4
let bit_arc_has_final_output = 1 lsl 5
let check_flag byte flag = byte land flag != 0

let is_bit_set_int64 n b =
  Int64.logand n (Int64.shift_left 1L b) <> 0L

let output_flags_num_bits = 2

let  bit_stop_node = 1 lsl 3

let no_output = ""

let index_of ~f l =
  let rec loop i l =
    match l with
    | [] -> None
    | x::rest -> if f x then Some i else loop (i + 1) rest in
  loop 0 l

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


module Block_term_state = struct
  type t = {
    doc_freq: int;
    total_term_freq: int;
    metadata_upto: int;
    doc_start_fp: int;
    pos_start_fp: int;
    pay_start_fp: int;
    singleton_doc_id: int option;
    last_pos_block_offset: int option;
    skip_offset: int option;
  }

  let init = {
      doc_freq = 0;
      total_term_freq = 0;
      metadata_upto = 0;
      doc_start_fp = 0;
      pos_start_fp = 0;
      pay_start_fp = 0;
      singleton_doc_id = None;
      last_pos_block_offset = None;
      skip_offset = None;
  }

  let show state =
    let {
      doc_freq; total_term_freq; metadata_upto; doc_start_fp; pos_start_fp;
      pay_start_fp; singleton_doc_id; last_pos_block_offset; skip_offset;
    } = state in
    Printf.sprintf "BlockTermState { doc_freq: %d; total_term_freq: %d; metadata_upto: %d; doc_start_fp: %d; pos_start_fp: %d; pay_start_fp: %d; singleton_doc_id: %d; last_pos_block_offset: %d; skip_offset: %d }"
     doc_freq total_term_freq metadata_upto doc_start_fp pos_start_fp pay_start_fp
     (match singleton_doc_id with | None -> -1 | Some n -> n)
     (match last_pos_block_offset with | None -> -1 | Some n -> n)
     (match skip_offset with | None -> -1 | Some n -> n)

end

module Index_iterator = struct
  type t = {
    term: string;
    prefix_length: int;
    suffixes: string list;
    stats_reader: String_data_input.t;
    postings_reader: String_data_input.t;
  }
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

let read_suffixes ~ent_count ~suffix_bytes ~suffix_length_bytes =
  let bytes_reader = String_data_input.from_string suffix_bytes in
  let length_bytes_reader = String_data_input.from_string suffix_length_bytes in
  let rec loop n =
    if n = 0 then
       []
    else
       let l = String_data_input.read_vint length_bytes_reader in
       let s = String_data_input.read_bytes bytes_reader l in
       s :: (loop (n - 1)) in
  loop ent_count

let debug_print_suffixes ~ent_count ~suffix_bytes ~suffix_length_bytes =
  let bytes_reader = String_data_input.from_string suffix_bytes in
  let length_bytes_reader = String_data_input.from_string suffix_length_bytes in
  let rec loop n =
    if n > 0 then
       let l = String_data_input.read_vint length_bytes_reader in
       let s = String_data_input.read_bytes bytes_reader l in
       Printf.printf "%d: %s\n" n s;
       loop (n - 1) in
  loop ent_count

let decode_postings_term ~field_info ~postings_reader ~term_state =

  (* Implemented for posting files with version VERSION_COMPRESSED_TERMS_DICT_IDS:6 or greater *)
  let code = String_data_input.read_vlong postings_reader in
  let {Block_term_state.doc_freq; singleton_doc_id; pos_start_fp; pay_start_fp; doc_start_fp; _} = term_state in
  let v = Int64.to_int (Int64.shift_right_logical code 1) in
  Printf.printf "V = %d\n" v;
  let has_doc_start_fp = not (is_bit_set_int64 code 0) in

  let doc_start_fp = doc_start_fp + (if has_doc_start_fp then v else 0) in
  Printf.printf "doc_start_fp = %d\n" doc_start_fp;
  let singleton_doc_id = if has_doc_start_fp then
    if doc_freq = 1 then Some (String_data_input.read_vint postings_reader)
    else None
  else
    Option.map (fun n -> n + v) singleton_doc_id in
  let field_has_positions = Field_infos.has_positions field_info in
  let field_has_offsets = Field_infos.has_offsets field_info in
  let field_has_payloads = Field_infos.has_payloads field_info in
  let pos_start_fp = pos_start_fp + (if field_has_positions then
    (Int64.to_int (String_data_input.read_vlong postings_reader))
  else
    (0)) in
  let pay_start_fp = pay_start_fp + (if field_has_positions && (field_has_offsets || field_has_payloads) then
    (Int64.to_int (String_data_input.read_vlong postings_reader))
  else
    (0)) in
  let last_pos_block_offset = if term_state.total_term_freq > 128 then
    Some (Int64.to_int (String_data_input.read_vlong postings_reader))
  else
    None in
  let skip_offset = if term_state.doc_freq > 128 then
    Some (Int64.to_int (String_data_input.read_vlong postings_reader))
  else
    None in
  { term_state with
    doc_freq;
    doc_start_fp;
    singleton_doc_id;
    pos_start_fp;
    pay_start_fp;
    last_pos_block_offset;
    skip_offset;
  }

let decode_metadata ~field_info ~limit ~stats_reader ~postings_reader=
  let rec loop ~singleton_run_length  ~term_state n =
    if n > limit then
      term_state
    else
      let (singleton_run_length, doc_freq, total_term_freq) = if singleton_run_length > 0 then
        (singleton_run_length - 1, 1, 1)
      else
        let token = String_data_input.read_vint stats_reader in
        let start_singleton_run = token land 1 == 1 in
        if start_singleton_run then
          let singleton_run_length = token lsr 1 in
          (singleton_run_length, 1, 1)
        else
          let doc_freq = token lsr 1 in
          let total_term_freq = (if Field_infos.has_freqs field_info then
            String_data_input.read_vint stats_reader
          else
            0) + doc_freq in
          (0, doc_freq, total_term_freq) in
      let term_state = {term_state with Block_term_state.doc_freq; total_term_freq; metadata_upto=(n+1)} in
      let term_state = decode_postings_term ~field_info ~postings_reader ~term_state in
      loop ~singleton_run_length ~term_state (n + 1) in
  let init_state = Block_term_state.init in
  loop ~singleton_run_length:0 ~term_state:init_state 0

let seek_exact ~block_tree_terms_reader ~field_reader ~fst target =
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
    if not has_terms then
      None
    else
      let terms_in = Index_input.copy (block_tree_terms_reader.Block_tree_terms_reader.terms_in) in
      Index_input.set_position terms_in fp;
      let code = Index_input.read_vint terms_in in
      let ent_count = code lsr 1 in
      assert (ent_count > 0);
      Printf.printf "Ent count = %d\n" ent_count;
      (* We only support newer segments with compressed suffixes ie >= v5 *)
      let code = Index_input.read_vlong terms_in in
      let is_leaf_block = Int64.logand code 4L <> 0L in
      let num_suffix_bytes = Int64.to_int (Int64.shift_right_logical code 3) in
      let compression_algo = Compression_algorithm.get_decompression_algo
        (Int64.to_int (Int64.logand code 3L)) in
      let suffix_bytes = compression_algo terms_in num_suffix_bytes in
      Printf.printf "is leaf block: %b, suffix bytes: %s\n" is_leaf_block suffix_bytes;
      let code = Index_input.read_vint terms_in in
      let num_suffix_length_bytes = code lsr 1 in
      let all_equal = code land 1 <> 0 in
      let suffix_length_bytes = if all_equal then
        let ch = Index_input.read_byte terms_in |> char_of_int in
        String.make num_suffix_length_bytes ch
      else
        Index_input.read_bytes terms_in num_suffix_length_bytes in
      Printf.printf "sufix length bytes: %d\n" (String.length suffix_length_bytes);
      let suffixes = read_suffixes ~ent_count ~suffix_bytes ~suffix_length_bytes in
      List.iter (fun s -> Printf.printf "s -> %s\n" s) suffixes;
      let stat_bytes = Index_input.read_string terms_in in
      let stats_reader = String_data_input.from_string stat_bytes in
      let postings_bytes = Index_input.read_string terms_in in
      let postings_reader = String_data_input.from_string postings_bytes in
      let target_suffix = String.sub target prefix_length ((String.length target) - prefix_length) in
      Log.debug (fun () -> Printf.sprintf "target suffix: %s\n" target_suffix);
      Assert.check_implemented is_leaf_block "if_leaf_block = false";
      let found_term = index_of ~f:(fun x -> x = target_suffix) suffixes in
      Log.debug (fun () -> Printf.sprintf "found term: %b" (Option.is_some found_term));
      match found_term with
      | None -> None
      | Some limit ->
      let it = Some {
        Index_iterator.term = target;
        prefix_length;
        suffixes;
        stats_reader: String_data_input.t;
        postings_reader: String_data_input.t;
      } in
      let field_info = field_reader.field_info in
      let block_state = decode_metadata ~field_info ~limit ~stats_reader ~postings_reader in

      Printf.printf "Block state = %s" (Block_term_state.show block_state);
      Some (it, block_state)


