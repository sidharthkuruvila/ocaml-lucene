open Lucene_data_input_2

module String_bytes_source = Bytes_source.Make(String_bytes)
module String_data_input = Data_input.Make(String_bytes_source)

let index_of ~f l =
  let rec loop i l =
    match l with
    | [] -> None
    | x::rest -> if f x then Some i else loop (i + 1) rest in
  loop 0 l


module Make(Data_input: Data_input.S) = struct




  module Block = struct
    type entry =
      | Term of String.t
      | Subblock of (String.t * int)

    type t = {
      entries: entry List.t;
      stat_bytes: String.t;
      postings_bytes: String.t;
    }
  end

  module Compression_algorithms = Compression_algorithms.Make(Data_input)

  let read_n ~f n =
    let rec loop n =
      if n = 0 then
        []
      else
        let s = f n in
        s :: loop (n - 1) in
    loop n

  let read_suffixes ~ent_count ~suffix_bytes ~suffix_length_bytes =
    let bytes_reader = String_bytes_source.of_bytes suffix_bytes in
    let length_bytes_reader = String_bytes_source.of_bytes suffix_length_bytes in
    read_n ent_count ~f:(fun _ ->
      let l = String_data_input.read_vint length_bytes_reader in
      String_data_input.read_bytes bytes_reader l
    )

  let read_block_items ~ent_count ~suffix_bytes ~suffix_length_bytes =
    let bytes_reader = String_bytes_source.of_bytes suffix_bytes in
    let length_bytes_reader = String_bytes_source.of_bytes suffix_length_bytes in
    read_n ent_count ~f:(fun _ ->
      let code = String_data_input.read_vint length_bytes_reader in
      let has_terms = code land 1 != 1 in
      let l = code lsr 1 in
      let suffix = String_data_input.read_bytes bytes_reader l in
      Printf.printf "has_Terms: %b, l: %d\n" has_terms l;
      if has_terms then
        Block.Term suffix
      else
        let sub_code = String_data_input.read_vlong length_bytes_reader in
        Block.Subblock (suffix, sub_code)
    )

  let read_block ~data_input =
    Printf.printf "Position = %d\n" (Data_input.get_position data_input);
    let code = Data_input.read_vint data_input in
    let ent_count = code lsr 1 in
    assert (ent_count > 0);
    Printf.printf "Ent count = %d\n" ent_count;
    (* We only support newer segments with compressed suffixes ie >= v5 *)
    let code = Data_input.read_vlong data_input in
    let is_leaf_block = code land 4 <> 0 in
(*    if not is_leaf_block then failwith "Unable to parse non leaf blocks";*)
    let num_suffix_bytes = code lsr 3 in
    let compression_algo = Compression_algorithms.get_decompression_algo (code land 3) in
    let suffix_bytes = compression_algo data_input num_suffix_bytes in
    Printf.printf "is leaf block: %b, suffix bytes length: %d, suffix bytes: %s\n" is_leaf_block (String.length suffix_bytes) suffix_bytes;
    let code = Data_input.read_vint data_input in
    Printf.printf "Code: %d\n" code;
    let num_suffix_length_bytes = code lsr 1 in
    let all_equal = code land 1 <> 0 in
    Printf.printf "all equal: %b, num suffix length bytes: %d \n" all_equal num_suffix_length_bytes;
    let suffix_length_bytes = if all_equal then
      let ch = Data_input.read_byte data_input in
      Printf.printf "Char = %d\n" (int_of_char ch);
      String.make num_suffix_length_bytes ch
    else
      Data_input.read_bytes data_input num_suffix_length_bytes in
    Printf.printf "sufix length bytes: %d, entcount: %d\n" (String.length suffix_length_bytes) ent_count;
    Printf.printf "suffix length bytes: %s\n" (String.concat " " (List.map (fun x -> string_of_int (int_of_char x)) (suffix_length_bytes |> String.to_seq |> List.of_seq)));
    let entries = if is_leaf_block then
      read_suffixes ~ent_count ~suffix_bytes ~suffix_length_bytes
        |> List.map (fun x -> Block.Term x)
    else
      read_block_items ~ent_count ~suffix_bytes ~suffix_length_bytes
    in
(*      List.iter (fun s -> Printf.printf "s -> %s\n" s) suffixes;*)
    let stat_bytes = Data_input.read_string data_input in
(*    let stats_reader = String_bytes_source.of_bytes stat_bytes in*)
    let postings_bytes = Data_input.read_string data_input in
(*    let postings_reader = String_bytes_source.of_bytes postings_bytes in*)
(*    assert is_leaf_block;*)
     { Block.
             entries;
             stat_bytes;
             postings_bytes;
     }

(*      failwith "Unable to parse non leaf blocks"*)
  let seek_term ~data_input position target_suffix =
    Data_input.set_position data_input position;
    let { Block.
      entries;
      stat_bytes;
      postings_bytes;
    } = read_block ~data_input in
    let suffixes = List.map (fun x -> match x with | Block.Term s -> s | _ -> failwith "no idea what to do") entries in
    let found_term = index_of ~f:(fun x -> x = target_suffix) suffixes in
    (
            suffixes,
            stat_bytes,
            postings_bytes,
            found_term
    )


end