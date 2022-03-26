open Lucene_data_output

module Data_output = Data_output.Make(Buffer_bytes_writer)

module Make(Output: Output.S)(Output_writer: Output_writer.S
  with type t = Output.t and type data_output =Data_output.t) = struct

  module Writer = Byte_array_fst_writer.Make(Data_output)(Output)(Output_writer)

  type transducer = (Buffer.t * int)

  module M = struct
    type 'a t = transducer -> ('a * transducer)
    let bind m f t = let (a, t) = (m t) in f a t
    let map m f t = let (a, t) = (m t) in (f a, t)
    let return a t = (a, t)
  end

  module Ops = Monad.Ops.Make(M)

  include M
  include Ops
  include Ops.Infix

  type state = (int * Output.t Option.t)
  module Output = Output

  open Writer

  let compile_state uncompiled_state (buffer, next_node) =
    let { State.final_output; transitions } = uncompiled_state in
    if Lists.is_empty transitions then
      match final_output with
      | None -> ((Output.empty, 0), (buffer, 0))
      | Some final_output -> ((final_output, -1), (buffer,  -1))
    else
      let final_output = Option.value final_output ~default:Output.empty in
      let arcs = List.map (fun {State.ch; output; target = (next_final_output, target) } ->
        {
          Arc.target; label = int_of_char ch; output;
          final_output = next_final_output;
        }
      ) transitions in
      let new_node = write_node buffer next_node arcs in
      ((final_output, new_node), (buffer, new_node))

  let run m =
    let transducer = (Buffer.create 1024, -1) in
    m transducer |> snd
end