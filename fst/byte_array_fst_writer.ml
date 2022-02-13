open Lucene_data_output

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

let set_flag_if cond flag =
  if cond then flag else 0

let rev_bytes bytes =
  let n = Bytes.length bytes - 1 in
  for i = 0 to (Bytes.length bytes - 1) / 2 do
    let t = Bytes.get bytes i in
    let u = Bytes.get bytes (n - i) in
   Bytes.set bytes (n - i) t;
   Bytes.set bytes i u
  done

module Buffer_data_output = Data_output.Make(Buffer_bytes_writer)

module type Buffer_output_writer = Output_writer.S with type data_output = Buffer.t

module Make (Data_output: Data_output.S)(Output: Output.S)
            (Output_writer: Buffer_output_writer with type t = Output.t) = struct

  module Arc = struct
    type t = {
      target: int;
      label: int;
      output: Output.t;
      final_output: Output.t;
    }
  end

  let write_linear_scan_node data_output next_node arcs =
    let last_arc_index = List.length arcs - 1 in
    let buffer = Buffer.create 10 in
    List.iteri (fun i arc ->
      let target = arc.Arc.target in
      let has_output = not (Output.is_empty arc.output) in
      let has_final_output = not (Output.is_empty arc.final_output) in
      let is_stop_node = target <= 0 in
      let target_is_next = target = next_node in
      let flags =
        (set_flag_if target_is_next bit_target_next lor
        set_flag_if has_output bit_arc_has_output lor
        set_flag_if has_final_output bit_arc_has_final_output lor
        set_flag_if (i = last_arc_index) bit_last_arc lor
        set_flag_if is_stop_node bit_stop_node lor
        set_flag_if (target = -1) bit_final_arc) |> char_of_int in
      Buffer_data_output.write_byte buffer flags;
      Buffer_data_output.write_byte buffer (char_of_int arc.label);
      if has_output then Output_writer.write buffer arc.output;
      if has_final_output then Output_writer.write buffer arc.final_output;
      if not (is_stop_node || target_is_next) then Buffer_data_output.write_vint buffer target;
      let bytes = Buffer.contents buffer |> Bytes.of_string in
      rev_bytes bytes;
      let reversed_node = Bytes.to_string bytes in
      Data_output.write_bytes data_output reversed_node
    ) arcs;
    Data_output.length data_output - 1


  let write_node data_output next_node arcs =
    write_linear_scan_node data_output next_node arcs
end