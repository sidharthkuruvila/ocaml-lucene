open Lucene_data_input
open Lucene_utils

module Log = Logger.Make(struct
 let name = "terms_enumerator"
end)


let is_bit_set_int64 n b =
  Int64.logand n (Int64.shift_left 1L b) <> 0L

let output_flags_num_bits = 2

let index_of ~f l =
  let rec loop i l =
    match l with
    | [] -> None
    | x::rest -> if f x then Some i else loop (i + 1) rest in
  loop 0 l

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
  } [@@deriving show]

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

end

module Index_iterator = struct
  type t = {
    term: string;
    prefix_length: int;
    suffixes: string list;
    stats_reader: String_data_input.t [@opaque];
    postings_reader: String_data_input.t [@opaque];
  } [@@deriving show]
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

let decode_metadata ~field_info ~limit ~stats_reader ~postings_reader =
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

let build_output path =
  let buf = Buffer.create 10 in
  let rec loop path =
    match path with
    | [] -> failwith "Empty path"
    | [last] ->
      Buffer.add_string buf last.Fst.Arc.output;
      Buffer.add_string buf last.Fst.Arc.final_output
    | x::rest ->
      Buffer.add_string buf x.Fst.Arc.output;
      loop rest in
  loop path;
  Bytes.to_string (Buffer.to_bytes buf)

let seek_exact ~block_tree_terms_reader ~field_reader ~fst target =
  print_endline "seeking exact";
  Printf.printf "target = %s size = %d\n" target (Field_reader.get_size field_reader);
  print_endline field_reader.min_term ;
  print_endline field_reader.max_term;
  if Field_reader.get_size field_reader > 0 && (String.compare target field_reader.min_term < 0 || String.compare target field_reader.max_term > 0) then
    None
  else
    let path = Fst.fst_match_term ~fst target in
    (* SegmentTermsEnum::pushFrame *)
    (* SegmentTermsEnumFrame::scanToFloorFrame *)
    (* The length of the prefix is the length of path traversed along the arcs minus 1 *)
    let prefix_length = List.length path - 1 in
    let target_label = int_of_char (String.get target prefix_length) in
    let output = build_output path in
    let (fp, has_terms) = find_block output target_label in
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
      Some (it, block_state)


