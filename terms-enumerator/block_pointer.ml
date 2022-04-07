open Lucene_data_input_2

let output_flags_num_bits = 2

module String_bytes_source = Bytes_source.Make(String_bytes)
module String_data_input = Data_input.Make(String_bytes_source)


let find_block output target_label =
  (* Get the file pointer for the block likely to contain the term
     the target_label is the chracter after the prefix. Each block contains terms
     for a range of target_label characters. For example if the target_label is g and
     the first block contains terms that would have matched for target_labels up to e
     we need to move to the next block. *)
  let output_reader = String_bytes_source.of_bytes output in
  let code = String_data_input.read_vlong output_reader in
  let fp = code lsr output_flags_num_bits in
  let has_terms = code land 2 <> 0 in
  let is_floor = code land 1 <> 0 in
  if not is_floor then
    if has_terms then Some fp else None
  else
    let num_floor_follow_blocks = String_data_input.read_vint output_reader in
    let next_floor_label = String_data_input.read_byte output_reader in
    if target_label < next_floor_label then begin
      if has_terms then Some fp else None
    end else
      let rec loop n fp =
        assert (n > 0);
        let code = String_data_input.read_vlong output_reader in
        let new_fp = fp + (code lsr 1) in
        let has_terms = code land 1 <> 0 in
        if n = 1 then
          if has_terms then Some new_fp else None
        else
          let next_floor_label = String_data_input.read_byte output_reader in
          if target_label < next_floor_label then
            if has_terms then Some new_fp else None
          else
            loop (n - 1) new_fp in
      loop num_floor_follow_blocks fp