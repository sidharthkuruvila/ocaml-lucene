open Lucene_utils
open Lucene_data_input

let rec fill n v =
  if n = 0 then []
  else v::(fill (n - 1) v)

module type S = sig
  type data_output_t
  val encode: int array -> data_output_t -> unit
end
let msb n =
  let rec loop i n c =
    if i = 0 then
      c
    else
      let (n, c) = if n > 1 lsl i
      then (n lsr i, c + i)
      else (n, c) in
      loop (i / 2) n c in
  loop 32 n 0

let top8_items arr =
  let l = [-1;-1;-1;-1;-1;-1;-1;-1] in
  let rec loop n l =
    if n = Array.length arr then
      l
    else
      loop (n - 1) (List.tl (List.sort (-) (n::l))) in
  loop 0 l


let rec last l =
  match l with
  | [] -> failwith "Last expects at least one item in the list"
  | [x] -> x
  | _::rest -> last rest



module Encode(Data_output: Data_output.S) = struct
  module Encode = Bit_packing.Encode(Data_output)
  let encode ints out = begin
    (* Find the 8 largest entries in the ints array *)
    let top8 = top8_items ints in
    (* Identify prefixes for the larger numbers that can be
       stored separately.
       The prefixes can be at most one byte long and will work for
       at most the top 7 items.

       The first step is to calculate the patched bits required, which is
       the number of bits left after the prefixes have been removed.
    *)
    let max_bits_required = Bit_utils.msb (last top8) in
    let patched_bits_required = max (Bit_utils.msb (List.hd top8)) (max_bits_required - 8) in
    let max_unpatched_value = (1 lsl patched_bits_required) - 1 in
    let exception_count = List.fold_left (fun a n -> if n > max_unpatched_value then a + 1 else a) 0 top8 in
    let exceptions = (Array.to_list ints) |> List.mapi (fun i a -> (i, a)) |> List.filter_map  (fun (i, a) ->
      if a > max_unpatched_value then Some(i, a land max_unpatched_value) else None) in
    let cropped_ints = Array.map (fun n -> n land max_unpatched_value) ints in
    let all_equal = Array.for_all (fun n -> cropped_ints.(0) = n) cropped_ints in
    if max_bits_required <= 8 && all_equal then begin
      Data_output.write_byte out (char_of_int (exception_count lsl 5));
      Data_output.write_vint out ints.(0);
      List.iter (fun (i, n) -> Data_output.write_byte out (char_of_int i); Data_output.write_byte out (char_of_int n)) exceptions
    end else begin
      Data_output.write_byte out (char_of_int ((exception_count lsl 5) lor patched_bits_required));
      Encode.encode (Array.to_list cropped_ints) max_unpatched_value out;
      List.iter (fun (i, n) -> Data_output.write_byte out (char_of_int i); Data_output.write_byte out (char_of_int n)) exceptions
    end;
  end
end

module Decode(Data_input: Data_input.S) = struct
  module Bit_packing = Bit_packing.Decode(Data_input)
  let decode din =
    let token = Data_input.read_byte din in
    let bits_per_value = token land 0x1f in
    let num_exception = token lsr 5 in
    let cropped_ints = if bits_per_value = 0 then
      fill 128 (Data_input.read_vint din)
    else
      Bit_packing.decode bits_per_value din in
    let ints = Array.of_list cropped_ints in
    for _ = 1 to num_exception do
      let index = Data_input.read_byte din in
      let value = Data_input.read_byte din in
      ints.(index) <- value
    done;
    ints
end