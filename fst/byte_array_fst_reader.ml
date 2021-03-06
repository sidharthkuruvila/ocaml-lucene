open Lucene_data_input_2

module Make(Data_input: Data_input.S)(Output: Output.S)
           (Output_reader: Output_reader.S with type data_input = Data_input.t and type t = Output.t) = struct

  module Output = Output

  let arcs_for_direct_addressing = 1 lsl 6
  let arcs_for_binary_search = 1 lsl 5
  let bit_last_arc = 1 lsl 1
  let bit_final_arc = 1 lsl 0

  (* The target node is the node following this one
     scan through the remaining nodes to get to the target *)
  let bit_target_next = 1 lsl 2

  let bit_arc_has_output = 1 lsl 4
  let bit_arc_has_final_output = 1 lsl 5
  let check_flag byte flag = byte land flag != 0

  let  bit_stop_node = 1 lsl 3

  type t = {
    di: Data_input.t;
    start_node: Int.t;
    empty_output: Output.t;
    start_position: Int.t;
  }

  let get_start_node t = t.start_node

  let first_arc t =
    Arc.{
      label = -1;
      target = t.start_node;
      output = Output.empty;
      final_output = t.empty_output;
    }

  let is_bit_set ~get_byte_at index =
    let byte = get_byte_at (index lsr 3) |> int_of_char in
    let bit_index = index land 7 in
    byte land (1 lsl bit_index) != 0


  let mask2 =  0x55
  let mask4 =  0x33
  let mask8 =  0x0F
  let count_byte_bits n =
    let count2 = ((n lsr 1) land mask2) + (n land mask2) in
    let count4 = ((count2 lsr 2) land mask4) + (count2 land mask4) in
    let count8 = ((count4 lsr 4) land mask8) + (count4 land mask8) in
    count8

  let range n =
    Seq.unfold (fun i -> if i < n then Some (i, i + 1) else None) 0

  let count_bits_upto ~get_byte_at index =
    let byte_index = index lsr 3 in
    let byte_range = range byte_index in
    let byte_counts = Seq.map (fun n ->
      let byte = get_byte_at n |> int_of_char in
      count_byte_bits byte) byte_range in
    let bytes_count = Seq.fold_left (fun a b -> a + b) 0 byte_counts in
    let remainder = index mod 8 in
    let mask = 1 lsl remainder - 1 in
    bytes_count + (count_byte_bits ((get_byte_at byte_index |> int_of_char) land mask))

  let count_bits ~get_byte_at byte_count =
    let byte_range = range byte_count in
    let byte_counts = Seq.map (fun n ->
      let byte = get_byte_at n |> int_of_char in
      count_byte_bits byte) byte_range in
    Seq.fold_left (fun a b -> a + b) 0 byte_counts


  let create ~di ~start_node ~empty_output =
    let start_position = Data_input.get_position di in
    let di_copy = Data_input.copy di in
    {
      di = di_copy;
      start_node;
      empty_output;
      start_position;
    }

  let is_bit_set arc_index ~input ~bit_table_start =
    Data_input.set_position input bit_table_start;
    let get_byte_at n =
      Data_input.get_byte input n in
    is_bit_set ~get_byte_at arc_index

  let count_bits_upto arc_index ~input ~bit_table_start =
    Data_input.set_position input bit_table_start;
    let get_byte_at n =
      Data_input.get_byte input n in
    count_bits_upto ~get_byte_at arc_index

  let count_bits byte_count ~input ~bit_table_start =
    Data_input.set_position input bit_table_start;
    let get_byte_at n =
      Data_input.get_byte input n in
    count_bits ~get_byte_at byte_count

  module Direct_addressing_node_info = struct
    type t = {
      num_arcs: int;
      bytes_per_arc: int;
      bit_table_start: int;
      presence_byte_count: int;
      first_label: int;
      arc_start: int;
    }
  end

  let read_direct_addressing_node_info ~input =
    let num_arcs = Data_input.read_vint input in
    let bytes_per_arc = Data_input.read_vint input in
    let bit_table_start = Data_input.get_position input in
    let presence_byte_count = (num_arcs + 7) lsr 3 in
    Data_input.skip_bytes input presence_byte_count;
    let first_label = Data_input.read_byte input |> int_of_char in
    let arc_start = Data_input.get_position input in
    { Direct_addressing_node_info.
      num_arcs;
      bytes_per_arc;
      bit_table_start;
      presence_byte_count;
      first_label;
      arc_start;
    }

  let read_direct_addressing_arc label { Direct_addressing_node_info.presence_byte_count; bit_table_start; bytes_per_arc; arc_start; _ } ~input =
    let flags = Data_input.read_byte input |> int_of_char in
    let output = if check_flag flags bit_arc_has_output then
        Output_reader.read input
      else
        Output.empty in
    let final_output = if check_flag flags bit_arc_has_final_output then
        Output_reader.read input
      else
        Output.empty in
    let is_stop_node = check_flag flags bit_stop_node in
    let is_final_arc = check_flag flags bit_final_arc in
    if is_stop_node then begin
      let next_arc = Some (Data_input.get_position input) in
      ({
         Arc.label;
         target = if is_final_arc then -1 else 0;
         output;
         final_output;
      }, next_arc)
    end else if check_flag flags bit_target_next then
      let target, next_arc = if check_flag flags bit_last_arc then
          (Data_input.get_position input, None)
        else
          let arc_count = count_bits presence_byte_count ~input ~bit_table_start in
          Printf.printf "arc count: %d\n" arc_count;
          (arc_start - bytes_per_arc * arc_count, Some (Data_input.get_position input)) in
      ({
        Arc.label;
        target;
        output;
        final_output;
      }, next_arc)
    else
      let target = Data_input.read_vlong input in
      let next_arc = Some (Data_input.get_position input) in
      ({
        Arc.label;
        target;
        output;
        final_output;
      }, next_arc)

  let find_direct_addressing_arc_for_label label node_info ~input =
    let { Direct_addressing_node_info.
      num_arcs;
      first_label;
      bit_table_start;
      arc_start;
      bytes_per_arc; _
    } = node_info in
    let arc_index = label - first_label in
    if arc_index < 0 || arc_index >= num_arcs then
      None
    else if not (is_bit_set arc_index ~input ~bit_table_start) then
      None
    else
      let presence_index = count_bits_upto arc_index ~input ~bit_table_start in
      Data_input.set_position input (arc_start - presence_index * bytes_per_arc);
      Some (read_direct_addressing_arc label node_info ~input)

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
    let node_info = read_direct_addressing_node_info ~input in
    let { Direct_addressing_node_info.
      presence_byte_count;
      _
    } = node_info in
    Data_input.skip_bytes input presence_byte_count;
    find_direct_addressing_arc_for_label label node_info ~input

  let read_label ~input =
    Data_input.read_byte input |> int_of_char

  let skip_to_next_arc ~flags ~input =
    if check_flag flags bit_arc_has_output then ignore (Output_reader.read input);
    if check_flag flags bit_arc_has_final_output then ignore (Output_reader.read input);
    if not(check_flag flags bit_stop_node) && not(check_flag flags bit_target_next) then ignore (Data_input.read_vint input);
    not (check_flag flags bit_last_arc)

  let seek_to_next_node ~input =
    let _ = input in
    failwith "not implemented yet"

  let read_linear_arc ~input ~flags =
    let label = read_label ~input in
    let output = if check_flag flags bit_arc_has_output then
        Output_reader.read input
      else
        Output.empty in
    let final_output = if check_flag flags bit_arc_has_final_output then
        Output_reader.read input
      else
        Output.empty in
    let is_stop_node = check_flag flags bit_stop_node in
    let is_final_arc = check_flag flags bit_final_arc in
    if is_stop_node then
      let next_arc = if check_flag flags bit_last_arc then None else Some (Data_input.get_position input) in
      ({
         Arc.label;
         target = if is_final_arc then -1 else 0;
         output;
         final_output;
      }, next_arc)
    else if check_flag flags bit_target_next then
      let next_arc = if check_flag flags bit_last_arc then None else Some (Data_input.get_position input) in
      let target = if check_flag flags bit_last_arc then
        Data_input.get_position input
      else
        seek_to_next_node ~input in
      ({
        Arc.label;
        target;
        output;
        final_output;
      }, next_arc)
    else
      let target = Data_input.read_vint input in
      let next_arc = if check_flag flags bit_last_arc then None else Some (Data_input.get_position input) in
      ({
        Arc.label;
        target;
        output;
        final_output;
      }, next_arc)

  let rec next_arc_using_linear_scan label ~flags ~input =
    let arc_position = Data_input.get_position input in
    let arc_label = read_label ~input in
    if arc_label = label then begin
      Data_input.set_position input arc_position;
      Some (read_linear_arc ~input ~flags)
    end else
      let has_more_arcs = skip_to_next_arc ~flags ~input in
      let flags = Data_input.read_byte input |> int_of_char in
      if has_more_arcs then next_arc_using_linear_scan label ~flags ~input
      else None

  module Binary_search_node_info = struct
    type t = {
      num_arcs: int;
      bytes_per_arc: int;
      arc_start: int;
    }
  end

  let read_binary_search_node_info ~input =
    let num_arcs = Data_input.read_vint input in
    let bytes_per_arc = Data_input.read_vint input in
    let arc_start = Data_input.get_position input in
    { Binary_search_node_info.
      num_arcs;
      bytes_per_arc;
      arc_start
    }

  let next_arc_using_binary_search label ~input =
    let node_info = read_binary_search_node_info ~input in
    let { Binary_search_node_info.num_arcs; bytes_per_arc; arc_start } = node_info in
    let low = 0 in
    let high = num_arcs in
    let rec search low high =
       if low >= high  then
         None
       else
         let mid = (low + high) / 2 in
         Data_input.set_position input (arc_start - bytes_per_arc * mid - 1);
         let cur_label = read_label ~input in
         Printf.printf "cur label = %c; label = %c\n" (char_of_int cur_label) (char_of_int label);
         if cur_label = label then begin
           Printf.printf "should be equal cur label = %c; label = %c\n" (char_of_int cur_label) (char_of_int label);
           Data_input.set_position input (arc_start - bytes_per_arc * mid);
           let flags = Data_input.read_byte input |> int_of_char in
           Some (read_linear_arc ~input ~flags)
         end else if cur_label < label then
            search (mid + 1) high
         else
            search low mid in
    search low high

  let use_node_search_strategy ~input target
    ~use_direct_addressing
    ~use_binary_search
    ~use_linear_scan =
    assert (target > 0 );
    Data_input.set_position input target;
    let flags = Data_input.read_byte input |> int_of_char in
    if flags = arcs_for_direct_addressing then
      use_direct_addressing ()
    else if flags = arcs_for_binary_search then
      use_binary_search ()
    else
      use_linear_scan flags

  let read_next_arc label ~fst_reader ~arc =
    let input = fst_reader.di in
    let use_direct_addressing () =
      next_arc_using_direct_addressing label ~input in
    let use_binary_search () =
      next_arc_using_binary_search label ~input in
    let use_linear_scan flags =
       next_arc_using_linear_scan label ~flags ~input in
    let target = arc.Arc.target in
    use_node_search_strategy ~input target
      ~use_direct_addressing
      ~use_binary_search
      ~use_linear_scan

  let read_linear_arcs_at_target ~flags ~input =
    let rec loop flags =
      Printf.printf "inside loop\n";
      let (arc, next_arc) = read_linear_arc ~flags ~input in
      match next_arc with
      | Some _ ->
         let flags = Data_input.read_byte input |> int_of_char in
         arc::(loop flags)
      | None -> [arc] in
    loop flags

  let read_direct_addressing_arcs_at_target ~input =
    let node_info = read_direct_addressing_node_info ~input in
    let { Direct_addressing_node_info.num_arcs; arc_start; bytes_per_arc; first_label; _ } = node_info in
    let rec loop n =
      if n = num_arcs then
        []
      else
        let arc_position = arc_start + n * bytes_per_arc in
        Data_input.set_position input arc_position;
        let label = first_label + n in
        let next_arc = find_direct_addressing_arc_for_label label node_info ~input in
        match next_arc with
        | None -> loop (n + 1)
        | Some (arc, _) -> arc :: loop (n + 1) in
    loop 0

  let read_binary_search_arcs_at_target ~input =
    let node_info = read_binary_search_node_info ~input in
    let { Binary_search_node_info.num_arcs; bytes_per_arc; arc_start } = node_info in
    let rec loop n =
      if n = num_arcs then
        []
      else
        let arc_pos = arc_start - bytes_per_arc*n in
        Data_input.set_position input arc_pos;
        let flags = Data_input.read_byte input |> int_of_char in
        let arc = read_linear_arc ~input ~flags |> fst in
        arc :: loop (n + 1) in
    loop 0

  let read_arcs_at_target ~fst_reader target =
    let input = fst_reader.di in
    let use_direct_addressing () =
      read_direct_addressing_arcs_at_target ~input in
    let use_binary_search () =
      read_binary_search_arcs_at_target ~input in
    let use_linear_scan flags =
      read_linear_arcs_at_target ~flags ~input in
    use_node_search_strategy ~input target
      ~use_direct_addressing
      ~use_binary_search
      ~use_linear_scan
end